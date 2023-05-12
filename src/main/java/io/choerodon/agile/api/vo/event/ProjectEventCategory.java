package io.choerodon.agile.api.vo.event;


/**
 * @author superlee
 * @since 2021-01-08
 */
public class ProjectEventCategory {

    private Long id;
    private String name;
    private String description;
    private String code;

    private Long organizationId;

    private Boolean displayFlag;

    private Boolean builtInFlag;

    private String labelCode;

    public Long getId() {
        return id;
    }

    public ProjectEventCategory setId(Long id) {
        this.id = id;
        return this;
    }

    public String getName() {
        return name;
    }

    public ProjectEventCategory setName(String name) {
        this.name = name;
        return this;
    }

    public String getDescription() {
        return description;
    }

    public ProjectEventCategory setDescription(String description) {
        this.description = description;
        return this;
    }

    public String getCode() {
        return code;
    }

    public ProjectEventCategory setCode(String code) {
        this.code = code;
        return this;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public ProjectEventCategory setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
        return this;
    }

    public Boolean getDisplayFlag() {
        return displayFlag;
    }

    public ProjectEventCategory setDisplayFlag(Boolean displayFlag) {
        this.displayFlag = displayFlag;
        return this;
    }

    public Boolean getBuiltInFlag() {
        return builtInFlag;
    }

    public ProjectEventCategory setBuiltInFlag(Boolean builtInFlag) {
        this.builtInFlag = builtInFlag;
        return this;
    }

    public String getLabelCode() {
        return labelCode;
    }

    public ProjectEventCategory setLabelCode(String labelCode) {
        this.labelCode = labelCode;
        return this;
    }
}
