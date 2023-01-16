package io.choerodon.agile.infra.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import io.choerodon.agile.infra.utils.StringUtil;
import io.choerodon.mybatis.domain.AuditDomain;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/5/24
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserMessageDTO extends AuditDomain {
    private String name;
    private String loginName;
    private String realName;
    private String imageUrl;
    private String email;
    private Boolean ldap;
    @Encrypt
    private Long id;
    @ApiModelProperty("attribute14, 目前用做用real_name的拼音首字母大写")
    private String attribute14;
    @ApiModelProperty("attribute15, 目前用做用real_name的拼音小写")
    private String attribute15;

    public UserMessageDTO() {

    }

    public UserMessageDTO(String name, String imageUrl, String email) {
        this.name = name;
        this.imageUrl = imageUrl;
        this.email = email;
    }

    public UserMessageDTO(String name, String loginName, String realName, String imageUrl, String email, Boolean ldap) {
        this.name = name;
        this.loginName = loginName;
        this.realName = realName;
        this.imageUrl = imageUrl;
        this.email = email;
        this.ldap = ldap;
    }

    public UserMessageDTO(String name, String loginName, String realName,
                          String imageUrl, String email, Boolean ldap, Long id) {
        this.name = name;
        this.loginName = loginName;
        this.realName = realName;
        this.imageUrl = imageUrl;
        this.email = email;
        this.ldap = ldap;
        this.id = id;
    }

    public String getLoginName() {

        return loginName;
    }

    public void setLoginName(String loginName) {
        this.loginName = loginName;
    }

    public String getRealName() {
        return realName;
    }

    public void setRealName(String realName) {
        this.realName = realName;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getImageUrl() {
        return imageUrl;
    }

    public void setImageUrl(String imageUrl) {
        this.imageUrl = imageUrl;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public void setLdap(Boolean ldap) {
        this.ldap = ldap;
    }

    public Boolean getLdap() {
        return ldap;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

    /**
     * @return attribute14, 目前用做用real_name的拼音首字母大写
     */
    public String getAttribute14() {
        return attribute14;
    }

    public void setAttribute14(String attribute14) {
        this.attribute14 = attribute14;
    }

    /**
     * @return attribute15, 目前用做用real_name的拼音小写
     */
    public String getAttribute15() {
        return attribute15;
    }

    public void setAttribute15(String attribute15) {
        this.attribute15 = attribute15;
    }
}
