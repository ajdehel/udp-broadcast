#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

extern "C" {
int get_socket( void );
int get_broadcast_socket( void );

int bind_socket( int *sockfd, char *ip_host, short ip_port );

int send_msg( int sockfd, char *ip_host, short ip_port, char *msg, unsigned msg_len );
int broadcast_message( int sockfd, char *ip_host, short ip_port, char *msg, unsigned msg_len );

int recv_msg( int sockfd, char *msg, unsigned *msg_len, char *ip_host, unsigned short *ip_port );
}
