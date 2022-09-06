package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.math.BigDecimal;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/6/28
 */
public class IssueWorkTimeCountVO {

    @ApiModelProperty(value = "issueId")
    @Encrypt
    private Long issueId;

    @ApiModelProperty(value = "工时日志数量")
    private Integer workLogCount;

    @ApiModelProperty(value = "工时数量")
    private BigDecimal workTimeCount;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Integer getWorkLogCount() {
        return workLogCount;
    }

    public void setWorkLogCount(Integer workLogCount) {
        this.workLogCount = workLogCount;
    }

    public BigDecimal getWorkTimeCount() {
        return workTimeCount;
    }

    public void setWorkTimeCount(BigDecimal workTimeCount) {
        this.workTimeCount = workTimeCount;
    }
}
