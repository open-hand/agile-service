package io.choerodon.agile.api.vo;


import io.swagger.annotations.ApiModelProperty;

import io.choerodon.agile.api.vo.search.SearchParamVO;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author shinan.chen
 * @since 2019/2/25
 */
public class PersonalFilterVO {

    @ApiModelProperty(value = "主键id")
    @Encrypt
    private Long filterId;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "用户id")
    @Encrypt
    private Long userId;

    @ApiModelProperty(value = "过滤名称")
    private String name;

    @ApiModelProperty(value = "搜索条件")
    private String filterJson;

    @ApiModelProperty(value = "版本号")
    private Long objectVersionNumber;

    @ApiModelProperty(value = "是否是默认")
    private Boolean isDefault;

    @ApiModelProperty(value = "个人筛选类型code")
    private String filterTypeCode;

    @ApiModelProperty(value = "组织id")
    private Long organizationId;
    @ApiModelProperty(value = "高级筛选条件")
    private SearchParamVO searchParamVO;

    public SearchParamVO getSearchParamVO() {
        return searchParamVO;
    }

    public void setSearchParamVO(SearchParamVO searchParamVO) {
        this.searchParamVO = searchParamVO;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Long getFilterId() {
        return filterId;
    }

    public void setFilterId(Long filterId) {
        this.filterId = filterId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getFilterJson() {
        return filterJson;
    }

    public void setFilterJson(String filterJson) {
        this.filterJson = filterJson;
    }

    public Boolean getDefault() {
        return isDefault;
    }

    public void setDefault(Boolean aDefault) {
        isDefault = aDefault;
    }

    public String getFilterTypeCode() {
        return filterTypeCode;
    }

    public void setFilterTypeCode(String filterTypeCode) {
        this.filterTypeCode = filterTypeCode;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }
}

