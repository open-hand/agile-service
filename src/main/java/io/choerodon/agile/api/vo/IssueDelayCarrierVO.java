package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.business.IssueDTO;

import java.util.Objects;

/**
 * @author superlee
 * @since 2021-03-08
 */
public class IssueDelayCarrierVO {

    private IssueDTO issueDTO;

    private Long issueId;

    private Long delayDay;

    private Long organizationId;

    public IssueDelayCarrierVO(IssueDTO issueDTO,
                               Long issueId,
                               Long delayDay,
                               Long organizationId) {
        this.issueDTO = issueDTO;
        this.issueId = issueId;
        this.delayDay = delayDay;
        this.organizationId = organizationId;
    }

    public IssueDTO getIssueDTO() {
        return issueDTO;
    }

    public void setIssueDTO(IssueDTO issueDTO) {
        this.issueDTO = issueDTO;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getDelayDay() {
        return delayDay;
    }

    public void setDelayDay(Long delayDay) {
        this.delayDay = delayDay;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IssueDelayCarrierVO)) return false;
        IssueDelayCarrierVO that = (IssueDelayCarrierVO) o;
        return getIssueId().equals(that.getIssueId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getIssueId());
    }
}
