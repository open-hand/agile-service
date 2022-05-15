package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotNull;

import io.choerodon.mybatis.domain.AuditDomain;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/13 15:59
 */

public class FieldCascadeRuleOptionVO extends AuditDomain {

    @Encrypt
    @ApiModelProperty("id")
    private Long id;
    @Encrypt
    @ApiModelProperty("字段级联规则id")
    private Long fieldCascadeRuleId;
    @Encrypt
    @ApiModelProperty("级联选项id")
    private Long cascadeOptionId;
    @ApiModelProperty("级联选项名")
    private String cascadeOptionName;
    @ApiModelProperty("项目id")
    private Long projectId;
    @ApiModelProperty("默认选项")
    private Boolean defaultOption;
    @ApiModelProperty("组织id")
    private Long organizationId;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getFieldCascadeRuleId() {
        return fieldCascadeRuleId;
    }

    public void setFieldCascadeRuleId(Long fieldCascadeRuleId) {
        this.fieldCascadeRuleId = fieldCascadeRuleId;
    }

    public Long getCascadeOptionId() {
        return cascadeOptionId;
    }

    public void setCascadeOptionId(Long cascadeOptionId) {
        this.cascadeOptionId = cascadeOptionId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public Boolean getDefaultOption() {
        return defaultOption;
    }

    public void setDefaultOption(Boolean defaultOption) {
        this.defaultOption = defaultOption;
    }

    public String getCascadeOptionName() {
        return cascadeOptionName;
    }

    public void setCascadeOptionName(String cascadeOptionName) {
        this.cascadeOptionName = cascadeOptionName;
    }
}
