package io.choerodon.agile.infra.statemachineclient.dto;


import com.fasterxml.jackson.annotation.JsonIgnore;
import com.google.common.base.MoreObjects;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author peng.jiang@hand-china.com
 * @author dinghuang123@gmail.com
 * @since 2018/10/23
 */
public class ExecuteResult {

    private Boolean isSuccess;

    @Encrypt
    private Long resultStatusId;

    private String errorMessage;

    @JsonIgnore
    private Exception exception;
    @JsonIgnore
    private Boolean onlyUpdateRank;

    public ExecuteResult() {
    }

    public ExecuteResult(Boolean isSuccess, Long resultStatusId, String errorMessage) {
        this.isSuccess = isSuccess;
        this.resultStatusId = resultStatusId;
        this.errorMessage = errorMessage;
    }

    public Boolean getOnlyUpdateRank() {
        return onlyUpdateRank;
    }

    public void setOnlyUpdateRank(Boolean onlyUpdateRank) {
        this.onlyUpdateRank = onlyUpdateRank;
    }

    public Exception getException() {
        return exception;
    }

    public void setException(Exception exception) {
        this.exception = exception;
    }

    public Boolean getSuccess() {
        return isSuccess;
    }

    public void setSuccess(Boolean success) {
        isSuccess = success;
    }

    public Long getResultStatusId() {
        return resultStatusId;
    }

    public void setResultStatusId(Long resultStatusId) {
        this.resultStatusId = resultStatusId;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    @Override
    public String toString() {
        return MoreObjects.toStringHelper(this)
                .add("isSuccess", isSuccess)
                .add("resultStateId", resultStatusId)
                .add("errorMessage", errorMessage)
                .toString();
    }
}

