package io.choerodon.agile.infra.dto;


import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import io.choerodon.agile.infra.utils.StringUtil;
import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import org.hzero.starter.keyencrypt.core.Encrypt;
/**
 * 敏捷开发Issue标签
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 21:04:00
 */
@Table(name = "agile_issue_label")
@ModifyAudit
@VersionAudit
public class IssueLabelDTO extends AuditDomain {

    public IssueLabelDTO() {}

    public IssueLabelDTO(String labelName, Long projectId) {
        this.labelName = labelName;
        this.projectId = projectId;
    }

    /***/
    @Id
    @GeneratedValue
    @Encrypt
    private Long labelId;

    /**
     * 标签名称
     */
    private String labelName;

    /**
     * 项目id
     */
    private Long projectId;

    public Long getLabelId() {
        return labelId;
    }

    public void setLabelId(Long labelId) {
        this.labelId = labelId;
    }

    public String getLabelName() {
        return labelName;
    }

    public void setLabelName(String labelName) {
        this.labelName = labelName;
    }

    public Long getProjectId() {
        return projectId;
    }

    public IssueLabelDTO setProjectId(Long projectId) {
        this.projectId = projectId;
        return this;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

}
