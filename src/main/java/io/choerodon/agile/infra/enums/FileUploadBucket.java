package io.choerodon.agile.infra.enums;

/**
 * @author superlee
 * @since 2022-03-14
 */
public enum FileUploadBucket {

    AGILE_BUCKET("agile-service");

    private String bucket;

    FileUploadBucket(String bucket) {
        this.bucket = bucket;
    }

    public String bucket() {
        return this.bucket;
    }
}
