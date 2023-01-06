package io.choerodon.agile.api.vo.business;

import javax.persistence.Id;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

public class ProjectStatusVO {
    @Id
    @ApiModelProperty("主键Id")
    @Encrypt
    private Long id;
    @ApiModelProperty("组织Id")
    private Long tenantId;
    @ApiModelProperty("名称")
    private String name;
    @ApiModelProperty("所属流程：review项目审核/implement项目实施")
    private String process;
    @ApiModelProperty("设置为初始状态：1 是 0 否 ")
    private Boolean initialState;
    @ApiModelProperty("来源：system系统/custom组织")
    private String source;
    @ApiModelProperty("状态：1 启用 0 停用")
    private Boolean enable;
    @ApiModelProperty("颜色十六进制码")
    private String color;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getTenantId() {
        return tenantId;
    }

    public void setTenantId(Long tenantId) {
        this.tenantId = tenantId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getProcess() {
        return process;
    }

    public void setProcess(String process) {
        this.process = process;
    }

    public Boolean getInitialState() {
        return initialState;
    }

    public void setInitialState(Boolean initialState) {
        this.initialState = initialState;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public Boolean getEnable() {
        return enable;
    }

    public void setEnable(Boolean enable) {
        this.enable = enable;
    }

    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }
}
