package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author chihao.ran@hand-china.com
 * 2021/03/17 9:34
 */
public class AppVersionRelProductSequenceVO {
    @ApiModelProperty(value = "应用版本id")
    private Long appVersionId;
    @ApiModelProperty(value = "产品版本排序字段")
    private Integer sequence;

    public Long getAppVersionId() {
        return appVersionId;
    }

    public void setAppVersionId(Long appVersionId) {
        this.appVersionId = appVersionId;
    }

    public Integer getSequence() {
        return sequence;
    }

    public void setSequence(Integer sequence) {
        this.sequence = sequence;
    }
}
