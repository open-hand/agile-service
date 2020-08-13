package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.StatusFieldValueSettingDTO;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;
/**
 * @author zhaotianxin
 * @date 2020-08-13 14:48
 */
public class StatusFieldSettingVO {
    @Encrypt
    private Long id;

    @Encrypt
    private Long issueTypeId;

    @Encrypt
    private Long statusId;

    private Long projectId;

    @Encrypt
    private Long fieldId;

    private List<StatusFieldValueSettingDTO> fieldValueList;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getFieldId() {
        return fieldId;
    }

    public void setFieldId(Long fieldId) {
        this.fieldId = fieldId;
    }

    public List<StatusFieldValueSettingDTO> getFieldValueList() {
        return fieldValueList;
    }

    public void setFieldValueList(List<StatusFieldValueSettingDTO> fieldValueList) {
        this.fieldValueList = fieldValueList;
    }
}
