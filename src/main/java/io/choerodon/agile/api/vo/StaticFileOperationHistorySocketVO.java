package io.choerodon.agile.api.vo;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/21 16:07
 */
public class StaticFileOperationHistorySocketVO {
    private String action;
    private String status;
    private String errorMessage;
    private Double process;
    private String fileName;

    public String getAction() {
        return action;
    }

    public void setAction(String action) {
        this.action = action;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    public Double getProcess() {
        return process;
    }

    public void setProcess(Double process) {
        this.process = process;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }
}
