package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotNull;

/**
 * @author huaxin.deng@hand-china.com 2021-02-02 13:54:38
 */
public class PersonalTemplateUpdateVO {

    @ApiModelProperty(value = "excel模板id")
    @NotNull(message = "error.template.idNotNull")
    @Encrypt
    private Long id;

    @ApiModelProperty(value = "模板名称")
    private String name;

    @ApiModelProperty(value = "模板json")
    private String templateJson;

    @ApiModelProperty(value = "版本号")
    private Long objectVersionNumber;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getTemplateJson() {
        return templateJson;
    }

    public void setTemplateJson(String templateJson) {
        this.templateJson = templateJson;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    @Override
    public String toString() {
        return "ExcelTemplateUpdateVO{" +
                "id=" + id +
                ", name='" + name + '\'' +
                ", templateJson='" + templateJson + '\'' +
                ", objectVersionNumber=" + objectVersionNumber +
                '}';
    }
}
