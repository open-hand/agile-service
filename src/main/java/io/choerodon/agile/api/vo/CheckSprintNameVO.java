package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author zhaotianxin
 * @since 2020/1/20
 */
public class CheckSprintNameVO {
    @ApiModelProperty("冲刺名称")
   private String sprintName;

    public String getSprintName() {
        return sprintName;
    }

    public void setSprintName(String sprintName) {
        this.sprintName = sprintName;
    }
}
