package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author zhaotianxin
 * @date 2021-08-04 11:30
 */
public class ExecutionLogQueryVO {

    @ApiModelProperty(value = "按执行状态筛选(LOOP/SUCCESS)")
    private String statusCode;

    @ApiModelProperty(value = "按issueNum或者概要筛选")
    private String params;

    public String getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
    }

    public String getParams() {
        return params;
    }

    public void setParams(String params) {
        this.params = params;
    }
}
