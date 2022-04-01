package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotNull;

/**
 * @author zhaotianxin
 * @date 2021-05-07 14:13
 */
public class ListLayoutColumnRelVO {
    @Encrypt
    private Long id;

    @Encrypt
    private Long layoutId;
    @Encrypt
    private Long fieldId;
    @NotNull(message = "error.layout.column.code.null")
    private String columnCode;
    @NotNull(message = "error.layout.column.width.null")
    private Integer width;
    @NotNull(message = "error.layout.column.sort.null")
    private Integer sort;
    @NotNull(message = "error.layout.column.display.null")
    private Boolean display;

    private String fieldProjectName;

    private Long projectId;

    private Long organizationId;

    private Long objectVersionNumber;

    @ApiModelProperty(value = "额外配置：工时包含子任务")
    private Boolean extraConfig;

    public String getFieldProjectName() {
        return fieldProjectName;
    }

    public void setFieldProjectName(String fieldProjectName) {
        this.fieldProjectName = fieldProjectName;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getLayoutId() {
        return layoutId;
    }

    public void setLayoutId(Long layoutId) {
        this.layoutId = layoutId;
    }

    public String getColumnCode() {
        return columnCode;
    }

    public void setColumnCode(String columnCode) {
        this.columnCode = columnCode;
    }

    public Integer getWidth() {
        return width;
    }

    public void setWidth(Integer width) {
        this.width = width;
    }

    public Integer getSort() {
        return sort;
    }

    public void setSort(Integer sort) {
        this.sort = sort;
    }

    public Boolean getDisplay() {
        return display;
    }

    public void setDisplay(Boolean display) {
        this.display = display;
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

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Long getFieldId() {
        return fieldId;
    }

    public void setFieldId(Long fieldId) {
        this.fieldId = fieldId;
    }

    public Boolean getExtraConfig() {
        return extraConfig;
    }

    public void setExtraConfig(Boolean extraConfig) {
        this.extraConfig = extraConfig;
    }
}
