package io.choerodon.agile.api.vo;


import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2021-06-08
 */
public class IssueTypeRankVO {

    @Encrypt
    private Long frontId;
    @Encrypt
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
