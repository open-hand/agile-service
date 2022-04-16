package io.choerodon.agile.api.vo;

public class BacklogDataVO {
    private Long backlogId;
    private Long backlogDataId;
    private String newValue;

    public Long getBacklogDataId() {
        return backlogDataId;
    }

    public void setBacklogDataId(Long backlogDataId) {
        this.backlogDataId = backlogDataId;
    }

    public Long getBacklogId() {
        return backlogId;
    }

    public void setBacklogId(Long backlogId) {
        this.backlogId = backlogId;
    }

    public String getNewValue() {
        return newValue;
    }

    public void setNewValue(String newValue) {
        this.newValue = newValue;
    }
}
