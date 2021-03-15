package io.choerodon.agile.api.vo;


import io.choerodon.agile.api.vo.business.ProgramVersionInfoVO;
import io.choerodon.agile.infra.dto.UserMessageDTO;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;
import java.util.List;

/**
 * Created by jian_zhang02@163.com on 2018/5/14.
 */

public class ProductVersionPageVO {

    @ApiModelProperty(value = "版本主键id")
    @Encrypt
    private Long versionId;

    @ApiModelProperty(value = "版本名称")
    private String name;

    @ApiModelProperty(value = "版本描述")
    private String description;

    @ApiModelProperty(value = "版本开始时间")
    private Date startDate;

    @ApiModelProperty(value = "版本预计发布时间")
    private Date expectReleaseDate;

    @ApiModelProperty(value = "版本发布时间")
    private Date releaseDate;

    @ApiModelProperty(value = "版本状态code")
    private String statusCode;

    @ApiModelProperty(value = "版本状态名称")
    private String status;

    @ApiModelProperty(value = "版本排序字段")
    private Integer sequence;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "版本号")
    private Long objectVersionNumber;

    @ApiModelProperty(value = "创建时间")
    private Long creationDate;

    @ApiModelProperty(value = "创建人id")
    private Long creationBy;

    @ApiModelProperty(value = "创建人")
    private UserMessageDTO creationUser;

    @ApiModelProperty(value = "子项目版本关联的项目群版本")
    private List<ProgramVersionInfoVO> programVersionInfoVOS;

    public Long getVersionId() {
        return versionId;
    }

    public void setVersionId(Long versionId) {
        this.versionId = versionId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public void setExpectReleaseDate(Date expectReleaseDate) {
        this.expectReleaseDate = expectReleaseDate;
    }

    public Date getExpectReleaseDate() {
        return expectReleaseDate;
    }

    public Date getReleaseDate() {
        return releaseDate;
    }

    public void setReleaseDate(Date releaseDate) {
        this.releaseDate = releaseDate;
    }

    public String getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Integer getSequence() {
        return sequence;
    }

    public void setSequence(Integer sequence) {
        this.sequence = sequence;
    }

    public List<ProgramVersionInfoVO> getProgramVersionInfoVOS() {
        return programVersionInfoVOS;
    }

    public void setProgramVersionInfoVOS(List<ProgramVersionInfoVO> programVersionInfoVOS) {
        this.programVersionInfoVOS = programVersionInfoVOS;
    }

    public Long getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(Long creationDate) {
        this.creationDate = creationDate;
    }

    public Long getCreationBy() {
        return creationBy;
    }

    public void setCreationBy(Long creationBy) {
        this.creationBy = creationBy;
    }

    public UserMessageDTO getCreationUser() {
        return creationUser;
    }

    public void setCreationUser(UserMessageDTO creationUser) {
        this.creationUser = creationUser;
    }
}
