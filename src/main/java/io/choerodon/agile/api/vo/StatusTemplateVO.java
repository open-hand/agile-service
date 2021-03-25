package io.choerodon.agile.api.vo;

/**
 * @author zhaotianxin
 * @date 2021-03-25 10:50
 */
public class StatusTemplateVO {

    private Long statusId;

    private String name;

    private String categoryCode;

    private Integer position;

    private Boolean templateCompleted;

    private Long organizationId;

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getCategoryCode() {
        return categoryCode;
    }

    public void setCategoryCode(String categoryCode) {
        this.categoryCode = categoryCode;
    }

    public Integer getPosition() {
        return position;
    }

    public void setPosition(Integer position) {
        this.position = position;
    }

    public Boolean getTemplateCompleted() {
        return templateCompleted;
    }

    public void setTemplateCompleted(Boolean templateCompleted) {
        this.templateCompleted = templateCompleted;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }
}
