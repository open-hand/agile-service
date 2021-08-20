package io.choerodon.agile.api.vo;

/**
 * @author zhaotianxin
 * @date 2021-08-19 10:34
 */
public class LinkIssueLinkageMessageVO {

    private String key;

    private String statusCode;

    private String message;

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public String getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}
