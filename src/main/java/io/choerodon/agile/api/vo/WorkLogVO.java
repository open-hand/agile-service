package io.choerodon.agile.api.vo;


import io.choerodon.agile.infra.constants.EncryptionConstant;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.math.BigDecimal;
import java.util.Date;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/18.
 * Email: fuqianghuang01@gmail.com
 */
public class WorkLogVO {

    @ApiModelProperty(value = "工作日志主键id")
    @Encrypt/*(EncryptionConstant.AGILE_WORK_LOG)*/
    private Long logId;

    @ApiModelProperty(value = "工作时间")
    private BigDecimal workTime;

    @ApiModelProperty(value = "开始时间")
    private Date startDate;

    @ApiModelProperty(value = "描述")
    private String description;

    @ApiModelProperty(value = "问题id")
    @Encrypt/*(EncryptionConstant.AGILE_ISSUE)*/
    private Long issueId;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "版本号")
    private Long objectVersionNumber;

    @ApiModelProperty(value = "剩余的估计类别字段")
    private String residualPrediction;

    @ApiModelProperty(value = "预估时间")
    private BigDecimal predictionTime;

    @ApiModelProperty(value = "用户id")
    private Long createdBy;

    @ApiModelProperty(value = "用户名称")
    private String userName;

    @ApiModelProperty(value = "最后更新时间")
    private Date lastUpdateDate;

    @ApiModelProperty(value = "用户头像url")
    private String userImageUrl;

    @ApiModelProperty(value = "用户真实姓名")
    private String realName;

    @ApiModelProperty(value = "用户登录名")
    private String loginName;

    public String getUserImageUrl() {
        return userImageUrl;
    }

    public void setUserImageUrl(String userImageUrl) {
        this.userImageUrl = userImageUrl;
    }

    public String getRealName() {
        return realName;
    }

    public void setRealName(String realName) {
        this.realName = realName;
    }

    public String getLoginName() {
        return loginName;
    }

    public void setLoginName(String loginName) {
        this.loginName = loginName;
    }

    public Long getLogId() {
        return logId;
    }

    public void setLogId(Long logId) {
        this.logId = logId;
    }

    public void setWorkTime(BigDecimal workTime) {
        this.workTime = workTime;
    }

    public BigDecimal getWorkTime() {
        return workTime;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public Date getStartDate() {
        return startDate;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setResidualPrediction(String residualPrediction) {
        this.residualPrediction = residualPrediction;
    }

    public String getResidualPrediction() {
        return residualPrediction;
    }

    public void setPredictionTime(BigDecimal predictionTime) {
        this.predictionTime = predictionTime;
    }

    public BigDecimal getPredictionTime() {
        return predictionTime;
    }

    public void setCreatedBy(Long createdBy) {
        this.createdBy = createdBy;
    }

    public Long getCreatedBy() {
        return createdBy;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public Date getLastUpdateDate() {
        return lastUpdateDate;
    }

    public void setLastUpdateDate(Date lastUpdateDate) {
        this.lastUpdateDate = lastUpdateDate;
    }
}
