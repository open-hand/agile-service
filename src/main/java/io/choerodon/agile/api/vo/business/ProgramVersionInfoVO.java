package io.choerodon.agile.api.vo.business;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2020-11-16 11:20
 */
public class ProgramVersionInfoVO {

    @Encrypt
    private Long id;

    private String name;

    @Encrypt
    private Long productVersionId;

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

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Long getProductVersionId() {
        return productVersionId;
    }

    public void setProductVersionId(Long productVersionId) {
        this.productVersionId = productVersionId;
    }
}
