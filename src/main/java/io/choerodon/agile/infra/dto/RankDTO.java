package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import javax.persistence.GeneratedValue;

import javax.persistence.Id;
import javax.persistence.Table;

@Table(name = "agile_rank")
@ModifyAudit
@VersionAudit
public class RankDTO extends AuditDomain {

    public RankDTO() {}

    public RankDTO(Long issueId, String rank) {
        this.issueId = issueId;
        this.rank = rank;
    }

    public RankDTO(Long id, String rank, Long objectVersionNumber) {
        this.id = id;
        this.rank = rank;
        this.objectVersionNumber = objectVersionNumber;
    }

    @Id
    @GeneratedValue
    private Long id;

    private Long projectId;

    private String type;

    private Long issueId;

    private String rank;

    private Long objectVersionNumber;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public String getRank() {
        return rank;
    }

    public void setRank(String rank) {
        this.rank = rank;
    }

    @Override
    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    @Override
    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }
}
