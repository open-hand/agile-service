package io.choerodon.agile.infra.dto.business;


import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/4/2.
 * Email: fuqianghuang01@gmail.com
 */
public class PiNameDTO {
    @Encrypt
    private Long id;

    private String code;

    private String name;

    private String statusCode;

    private Date startDate;

    private Date endDate;

    private Date actualStartDate;

    private Date actualEndDate;

    private Boolean hasSprint;

    private String fullName;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getCode() {
        return code;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
    }

    public String getStatusCode() {
        return statusCode;
    }

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    public Date getActualStartDate() {
        return actualStartDate;
    }

    public void setActualStartDate(Date actualStartDate) {
        this.actualStartDate = actualStartDate;
    }

    public Date getActualEndDate() {
        return actualEndDate;
    }

    public void setActualEndDate(Date actualEndDate) {
        this.actualEndDate = actualEndDate;
    }

    public Boolean getHasSprint() {
        return hasSprint;
    }

    public void setHasSprint(Boolean hasSprint) {
        this.hasSprint = hasSprint;
    }

    public String getFullName() {
        return fullName;
    }

    public void setFullName(String fullName) {
        this.fullName = fullName;
    }
}
