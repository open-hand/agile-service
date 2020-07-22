package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

public class WorkSpaceVO {
    @Encrypt
    private Long id;

    private String name;
    @Encrypt
    private Long baseId;

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
}
