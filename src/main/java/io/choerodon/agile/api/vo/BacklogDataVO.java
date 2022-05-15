package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

public class BacklogDataVO {
    @ApiModelProperty("需求id")
    private Long backlogId;
    @ApiModelProperty("需求数据id")
    private Long backlogDataId;
    @ApiModelProperty("新值")
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
