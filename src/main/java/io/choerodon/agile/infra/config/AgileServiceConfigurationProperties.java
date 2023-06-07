package io.choerodon.agile.infra.config;

import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * 敏捷服务配置属性
 * @author gaokuo.dai@zknow.com 2023-06-07
 */
@ConfigurationProperties(AgileServiceConfigurationProperties.CONFIGURATION_PREFIX)
public class AgileServiceConfigurationProperties {

    public static final String CONFIGURATION_PREFIX = "services";

    private Attachment attachment = new Attachment();
    private Domain domain = new Domain();
    private Chain chain = new Chain();

    public static class Attachment {
        private String bucketName = "private";
        private String directory = "agile-service";
        private String url = "http://minio.example.choerodon.io/private";

        public String getBucketName() {
            return bucketName;
        }

        public void setBucketName(String bucketName) {
            this.bucketName = bucketName;
        }

        public String getDirectory() {
            return directory;
        }

        public void setDirectory(String directory) {
            this.directory = directory;
        }

        public String getUrl() {
            return url;
        }

        public void setUrl(String url) {
            this.url = url;
        }
    }

    public static class Domain {
        private String url = StringUtils.EMPTY;

        public String getUrl() {
            return url;
        }

        public void setUrl(String url) {
            this.url = url;
        }
    }

    public static class Chain {
        private int maxDepth = 20;

        public int getMaxDepth() {
            return maxDepth;
        }

        public void setMaxDepth(int maxDepth) {
            this.maxDepth = maxDepth;
        }
    }

    public Attachment getAttachment() {
        return attachment;
    }

    public void setAttachment(Attachment attachment) {
        this.attachment = attachment;
    }

    public Domain getDomain() {
        return domain;
    }

    public void setDomain(Domain domain) {
        this.domain = domain;
    }

    public Chain getChain() {
        return chain;
    }

    public void setChain(Chain chain) {
        this.chain = chain;
    }

}
