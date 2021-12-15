package io.choerodon.agile.api.vo;


import java.util.Map;

/**
 * @author zhaotianxin
 * @date 2020-05-08 10:44
 */
public class BatchUpdateFieldStatusVO {

    private String key;

    private Long userId;

    private String status;

    private Double process;

    private String error;

    private Double incrementalValue;

    private Double lastProcess;

    private Map<Object, Object> errorMsgMap;

    private Integer successCount;

    private Integer failedCount;

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public Double getProcess() {
        return process;
    }

    public void setProcess(Double process) {
        this.process = process;
    }

    public String getError() {
        return error;
    }

    public void setError(String error) {
        this.error = error;
    }

    public Double getIncrementalValue() {
        return incrementalValue;
    }

    public void setIncrementalValue(Double incrementalValue) {
        this.incrementalValue = incrementalValue;
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Map<Object, Object> getErrorMsgMap() {
        return errorMsgMap;
    }

    public void setErrorMsgMap(Map<Object, Object> errorMsgMap) {
        this.errorMsgMap = errorMsgMap;
    }

    public Double getLastProcess() {
        return lastProcess;
    }

    public void setLastProcess(Double lastProcess) {
        this.lastProcess = lastProcess;
    }

    public Integer getSuccessCount() {
        return successCount;
    }

    public void setSuccessCount(Integer successCount) {
        this.successCount = successCount;
    }

    public Integer getFailedCount() {
        return failedCount;
    }

    public void setFailedCount(Integer failedCount) {
        this.failedCount = failedCount;
    }
}
