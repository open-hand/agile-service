package io.choerodon.agile.api.vo;


import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2021-06-08
 */
public class IssueTypeRankVO {

    @Encrypt
    @ApiModelProperty(value = "前一个id")
    private Long frontId;
    @Encrypt
    @ApiModelProperty(value = "后一个id")
    private Long backId;

    public Long getFrontId() {
        return frontId;
    }

    public void setFrontId(Long frontId) {
        this.frontId = frontId;
    }

    public Long getBackId() {
        return backId;
    }

    public void setBackId(Long backId) {
        this.backId = backId;
    }
}
