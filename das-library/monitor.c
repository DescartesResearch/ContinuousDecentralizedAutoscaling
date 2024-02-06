#define _GNU_SOURCE
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <stdio.h>
#include <curl/curl.h>
#include <math.h>
#include <sys/sysinfo.h>
#include <sched.h>
#include <pthread.h>
#include <getopt.h>


#define CHAR_BUFFER_LENGTH  256

FILE *f_ptr = NULL;
FILE *f_log = NULL;


uint32_t timespan = 60;
uint32_t t_lower = 30;
uint32_t t_upper = 90; // Exclusive

char log_path[CHAR_BUFFER_LENGTH] = "/var/log/das_monitor";
char api_endpoint_up[CHAR_BUFFER_LENGTH] = "http://192.168.2.34:40/up";
char api_endpoint_down[CHAR_BUFFER_LENGTH] = "http://192.168.2.34:40/down";
char api_endpoint_hold[CHAR_BUFFER_LENGTH] = "http://192.168.2.34:40/hold";
char proc_cpuacct_usage[CHAR_BUFFER_LENGTH] = "/sys/fs/cgroup/cpuacct/cpuacct.usage";

struct probabilities {
    double up;
    double down;
};

/* return a random number between 0 and limit exclusive. */
int rand_lim(uint32_t lower, uint32_t upper) {/*{{{*/
    upper--;
    uint32_t limit = upper - lower;
    uint32_t divisor = RAND_MAX/(limit+1);
    uint32_t retval;
    do {
        retval = random() / divisor;
    } while (retval > limit);

    return retval + lower;
}/*}}}*/

/* return random value between 0 and 1 */
double rand_double(){/*{{{*/
    return ((double)random()) / RAND_MAX;
}/*}}}*/

/* curl send get request at the target entpoint */
int8_t send_get(char * target, double up, double down)/*{{{*/
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, target);

    struct curl_slist *hs=NULL;
    hs = curl_slist_append(hs, "Content-Type: application/json");
    //hs = curl_slist_append(hs, "Up: 0.3");
    //hs = curl_slist_append(hs, "Down: 0.0");
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, hs);

    //curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 1L);
    //curl_easy_setopt(curl, CURLOPT_USERPWD, "user:pass");
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "curl/7.42.0");
    curl_easy_setopt(curl, CURLOPT_MAXREDIRS, 50L);
    FILE * fzero = fopen("/dev/zero", "w");
    // prevent writing data to stdout
    // CURLOPT_NOBODY prevents printing but sends only HEAD requests!

    char postthis[256];
    sprintf(postthis, "{\"up\":\"%.4f\", \"down\":\"%.4f\"}", up, down);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, postthis); //
    curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, (long)strlen(postthis)); //
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fzero);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL);
    curl_easy_setopt (curl, CURLOPT_VERBOSE, 0L); //0 disable messages
    //curl_easy_setopt(curl, CURLOPT_TCP_KEEPALIVE, 1L);

    /* Perform the request, res will get the return code */
    res = curl_easy_perform(curl);
    /* Check for errors */
    if(res != CURLE_OK) {
        fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        return 1;
    }

    /* always cleanup */
    curl_easy_cleanup(curl);
  }
  return 0;
}/*}}}*/

/* send upscale request to API endpoint */
void * send_scale_up(void * probp){/*{{{*/
    struct probabilities * prob = (struct probabilities *)probp;
    double up = prob->up;
    double down = prob->down;
    free(probp);
    send_get(api_endpoint_up, up, down);
}/*}}}*/

/* send downscale request to API endpoint */
void * send_scale_down(void * probp){/*{{{*/
    struct probabilities * prob = (struct probabilities *)probp;
    double up = prob->up;
    double down = prob->down;
    free(probp);
    send_get(api_endpoint_down, up, down);
}/*}}}*/

/* send status information to API endpoint when not up or downscaling */
void * send_scale_hold(void * probp){/*{{{*/
    struct probabilities * prob = (struct probabilities *)probp;
    double up = prob->up;
    double down = prob->down;
    free(probp);
    send_get(api_endpoint_hold, up, down);
}/*}}}*/

/* return the upscaling propability for the given load avg */
double prob_up(double load){/*{{{*/
    double threshold = 0.8;
        if (load < threshold) {
            return 0.0;
        }
    /* different rounding than in the r sim requires setting lower limit to 0 */
    return fmax( 0.0, fmin( 1.0, (0.024803L * exp(4.620981L * load)-1) ) );
}/*}}}*/

/* return the downscaling propability for the given load avg */
double prob_down(double load){/*{{{*/
    double threshold = 0.7;
        if (load >= threshold) {
            return 0.0;
        }
    /* different rounding than in the r sim requires setting lower limit to 0 */
    return fmax( 0.0, fmin( 1.0, (3.363586L * exp(-1.732868L * load)-1) ) );
}/*}}}*/

/* helper function to print the content of the load value array */
void print_array(uint64_t * timestamps){/*{{{*/
    for (unsigned int i = 0; i<timespan; i++){
        printf("%lu\t", timestamps[i]);
    }
    printf("\n");
}/*}}}*/

/* helper function to print values of the up and down scaling propabilities */
void print_values(){/*{{{*/
    printf("I       \t\tup       \tdown\n");
    for (double i=0.0; i<1.001; i=i+0.02){
        printf("%f:\t\t%f\t%f\n", i, prob_up(i), prob_down(i));
    }
}/*}}}*/

/* logging */
void logger(const char* msg) {
    if (f_log == NULL) {
        exit(9);
    }
    time_t t;
    time(&t);
    char timestr[CHAR_BUFFER_LENGTH];
    strftime(timestr, sizeof(timestr), "%Y-%m-%dT%H:%M:%SZ", gmtime(&t));
    fprintf(f_log,"[%s] %s\n", timestr, msg);
    fflush(f_log);
}

/* helper function to visualise rng */
void test_rand(){/*{{{*/
    uint64_t bucket = 100;
    uint64_t values_d[bucket+1];
    uint64_t values_i[bucket+1];
    memset(values_d,0,sizeof(values_d));
    memset(values_i,0,sizeof(values_i));
    for (uint32_t i=0; i<1000000; i++) {
        values_d[(int)(rand_double() * (double)bucket)]++;
        values_i[rand_lim(0,bucket)]++;
    }
    uint64_t sum_d = 0;
    uint64_t sum_i = 0;
    printf("Index:\tdouble \tint\n");
    for (uint32_t i=0; i<bucket+1; i++) {
        sum_d = sum_d + values_d[i];
        sum_i = sum_i + values_i[i];
        printf("%u:\t%lu, \t%lu\n", i, values_d[i], values_i[i]);
    }
    printf("Sum_d: %lu\n", sum_d);
    printf("Sum_i: %lu\n", sum_i);
}/*}}}*/

/* return number of cpus available */
int nprocs() {/*{{{*/
  cpu_set_t cs;
  CPU_ZERO(&cs);
  sched_getaffinity(0, sizeof(cs), &cs);
  return CPU_COUNT(&cs);
}/*}}}*/

/* read cfs quota from cgroup if available */
int get_cfs_quota() {/*{{{*/
    FILE * fp;
    if ((fp = fopen("/sys/fs/cgroup/cpuacct/cpu.cfs_quota_us", "r"))) {
        int ret = 0;
        fscanf(fp, "%d", &ret);
        return ret;
    }
    fprintf(stderr, "Could not open cpu.cfs_quota_us!");
    return -2;
}/*}}}*/

/* read cfs quota period from cgroup if available */
int get_cfs_period() {/*{{{*/
    FILE * fp;
    if ((fp = fopen("/sys/fs/cgroup/cpuacct/cpu.cfs_period_us", "r"))) {
        int ret = 0;
        fscanf(fp, "%d", &ret);
        return ret;
    }
    fprintf(stderr, "Could not open cpu.cfs_quota_us!");
    return -2;
}/*}}}*/


void routine(){
    uint64_t next_draw = rand_lim(t_lower,t_upper);
    uint32_t pass = 0;
    char msgbuffer[2* CHAR_BUFFER_LENGTH];
    memset(msgbuffer, 0, sizeof(msgbuffer));

    uint64_t load_values[timespan];
    memset(load_values, 0, sizeof(load_values));

    int32_t cpu_cores_avail = nprocs();
    sprintf(msgbuffer, "Cpus: %d", cpu_cores_avail);
    logger(msgbuffer);

    int64_t cfs_quota = get_cfs_quota();
    int64_t cfs_period = get_cfs_period();
    double ratio;

    if (cfs_quota > 0 && cfs_period > 0){
        ratio = (double)cfs_quota/(double)cfs_period;
    } else {
        ratio = 1.0;
    }
    sprintf(msgbuffer,"Proportion of cpu time available: %f\n", ratio);
    logger(msgbuffer);

    uint64_t value;
    double random;

    pthread_t thread;
    int iret = -1;
    int thread_running = 0;

    struct probabilities * prob;

    while (1) {

        f_ptr = fopen(proc_cpuacct_usage, "r");
        fscanf(f_ptr, "%ld", &value);
        fclose(f_ptr);

        if (thread_running == 1) {
            /* join pthread before proceeding with iteration, should have finished long time ago */
            pthread_join(thread, NULL);
            thread_running = 0;
        }

        /* Get average value of last $timespan sec. */
        if (pass++ == next_draw){
            next_draw = pass + rand_lim(t_lower,t_upper);

            // TODO: Maybe skip first iteration?
            prob = malloc(sizeof(struct probabilities));
            /* get avg cpuload */
            double cpusec_p_sec = ((double) value - (double)load_values[pass %timespan]) / timespan;
            cpusec_p_sec = cpusec_p_sec / 1000000000;
            cpusec_p_sec = cpusec_p_sec / ratio;
            sprintf(msgbuffer,"Avg load over last %d sec:\t%f", timespan, cpusec_p_sec);
            logger(msgbuffer);
            prob->up = prob_up(cpusec_p_sec);
            prob->down = prob_down(cpusec_p_sec);
            sprintf(msgbuffer,"Upscaling propability: %f", prob->up);
            logger(msgbuffer);
            sprintf(msgbuffer,"Downscaling propability: %f", prob->down);
            logger(msgbuffer);

            random = rand_double();
            if (prob->down >= random) {
                pthread_create(&thread, NULL, send_scale_down, prob);
                thread_running = 1;
                sprintf(msgbuffer,"Sending downscaling request to %s!", api_endpoint_down);
                logger(msgbuffer);
            } else if (prob->up >= random) {
                pthread_create(&thread, NULL, send_scale_up, prob);
                thread_running = 1;
                sprintf(msgbuffer, "Sending upscaling request to %s!", api_endpoint_up);
                logger(msgbuffer);
            } else {
                pthread_create(&thread, NULL, send_scale_hold, prob);
                thread_running = 1;
                sprintf(msgbuffer, "Sending scaling status to %s!", api_endpoint_hold);
                logger(msgbuffer);
            }

            sprintf(msgbuffer,"Next check after %lu sec.\n", next_draw - pass);
            logger(msgbuffer);
        }

        load_values[pass % timespan] = value;
        usleep(1000000);
    }
}

int main(int argc, char *argv[]){
    /* init randomness by seeding with microsecond portion of current time */
    struct timeval ts;
    gettimeofday(&ts, NULL);
    srand((unsigned int)(ts.tv_usec));
    rand();

    /* Check environment variables */
    char * buffer;
    if ((buffer = getenv("DAS_APIUP"))) {
        if (CHAR_BUFFER_LENGTH <= strlen(buffer)){
            printf("Option for DAS_APIUP ist too long!\n");
            return 7;
        }
        strcpy(api_endpoint_up, buffer);
    }
    if ((buffer = getenv("DAS_APIDOWN"))) {
        if (CHAR_BUFFER_LENGTH <= strlen(buffer)){
            printf("Option for DAS_APIDOWN ist too long!\n");
            return 7;
        }
        strcpy(api_endpoint_down, buffer);
    }
    if ((buffer = getenv("DAS_APIHOLD"))) {
        if (CHAR_BUFFER_LENGTH <= strlen(buffer)){
            printf("Option for DAS_APIHOLD ist too long!\n");
            return 7;
        }
        strcpy(api_endpoint_hold, buffer);
    }
    if ((buffer = getenv("DAS_LOGFILE"))) {
        if (CHAR_BUFFER_LENGTH <= strlen(buffer)){
            printf("Option for DAS_LOGFILE ist too long!\n");
            return 7;
        }
        strcpy(log_path, buffer);
    }
    if ((buffer = getenv("DAS_TFROM"))){
        t_lower = atoi(buffer);
    }
    if ((buffer = getenv("DAS_TTO"))){
        t_upper = atoi(buffer);
    }
    if ((buffer = getenv("DAS_TIMESPAN"))){
        timespan = atoi(buffer);
    }

    /* parse command line arguments */
    int c;
    static struct option long_options[] =
    {
        {"apiup", required_argument, 0, 'u'}, // API_ENDPOINT_UP
        {"apidown", required_argument, 0, 'd'}, // API_ENDPOINT_DOWN
        {"apihold", required_argument, 0, 'H'}, // API_ENDPOINT_HOLD
        {"tfrom", required_argument, 0, 'f'}, // lower bound for random timespan
        {"tto", required_argument, 0, 't'}, // upper bound for random timespan
        {"timespan", required_argument, 0, 's'}, // get avg load over this many seconds
        {"help", no_argument, 0, 'h'}, // get avg load over this many seconds
        {0, 0, 0, 0}
    };
    while (1) {
        int option_index = 0;
        c = getopt_long(argc, argv, "u:d:f:t:s:l:h", long_options, &option_index);
        if (c == -1)
            break;
        switch (c) {
            case 'h':
                printf("Help:\n");
                printf("  --apiup, -u\t\tDAS_APIUP\tSpecify api endpoint for upscaling\n");
                printf("  --apidown, -d\t\tDAS_APIUP\tSpecify api endpoint for downscaling\n");
                printf("  --apihold, -H\t\tDAS_APIHOLD\tSpecify api endpoint for status when not scaling\n");
                printf("  --tfrom, -f\t\tDAS_TFROM\tLower bound of random delay\n");
                printf("  --tto, -t\t\tDAS_TTO\t\tUpper bound of random delay\n");
                printf("  --timespan, -s\tDAS_TIMESPAN\tCalculate load average over this many seconds\n");
                printf("  --log, -l\t\tDAS_LOGFILE\tPath to the log file\n");
                printf("  --help, -h\t\t\t\tDisplay this message\n");
                printf("\n");
                return 0;
                break;
            case 'u':
                if (CHAR_BUFFER_LENGTH <= strlen(optarg)){
                    return 7;
                }
                strcpy(api_endpoint_up, optarg);
                break;
            case 'd':
                if (CHAR_BUFFER_LENGTH <= strlen(optarg)){
                    return 7;
                }
                strcpy(api_endpoint_down, optarg);
                break;
            case 'f':
                t_lower = atoi(optarg);
                break;
            case 't':
                t_upper = atoi(optarg);
                break;
            case 's':
                timespan = atoi(optarg);
                break;
            case 'l':
                if (CHAR_BUFFER_LENGTH <= strlen(optarg)){
                    return 7;
                }
                strcpy(log_path, optarg);
                break;

            case ':':
                /* handled by getopt_long */
                return 1;
                break;
            case '?':
                /* handled by getopt_long */
                return 1;
                break;
            default:
                abort();
        }
    }

    /* open log file */
    if ((f_log = fopen(log_path, "w"))) {
        printf("Logging to %s", log_path);
    } else {
        printf("Logfile not accessible");
        exit(8);
    }

    routine();
}


