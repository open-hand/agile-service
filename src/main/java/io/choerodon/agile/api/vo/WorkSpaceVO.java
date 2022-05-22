package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

public class WorkSpaceVO {
    @Encrypt
    @ApiModelProperty(value = "id")
    private Long id;
    @ApiModelProperty(value = "名称")
    private String name;
    @Encrypt
    @ApiModelProperty(value = "库id")
    private Long baseId;

    @ApiModelProperty("WorkSpace的类型")
    private String type;
    @ApiModelProperty("文件的类型")
    private String fileType;

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

    public Long getBaseId() {
        return baseId;
    }

    public void setBaseId(Long baseId) {
        this.baseId = baseId;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getFileType() {
        return fileType;
    }

    public void setFileType(String fileType) {
        this.fileType = fileType;
    }
}
