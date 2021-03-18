package io.choerodon.agile.api.vo;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.choerodon.agile.api.vo.report.ReportUnitVO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 下午5:53
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ProjectReportVO {


    @ApiModelProperty("主键id")
    @Encrypt
    private Long id;
    @ApiModelProperty("项目id")
    private Long projectId;
    @ApiModelProperty("报表标题")
    private String title;
    @ApiModelProperty("报表描述")
    private String description;
    @ApiModelProperty("状态")
    private String status;
    @ApiModelProperty("收件人List")
    private List<UserDTO> receiverList;
    @ApiModelProperty("抄送人List")
    private List<UserDTO> ccList;
    @ApiModelProperty("报表数据")
    private List<ReportUnitVO> reportUnitList;
    @ApiModelProperty("最近发送时间")
    private Date recentSendDate;
    @ApiModelProperty("创建人")
    @Encrypt
    private Long createdBy;
    @ApiModelProperty("创建人信息")
    private UserDTO createdUser;
    @ApiModelProperty("版本号")
    private Long objectVersionNumber;
    @ApiModelProperty("html字符串")
    private String html;

    public UserDTO getCreatedUser() {
        return createdUser;
    }

    public void setCreatedUser(UserDTO createdUser) {
        this.createdUser = createdUser;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Date getRecentSendDate() {
        return recentSendDate;
    }

    public void setRecentSendDate(Date recentSendDate) {
        this.recentSendDate = recentSendDate;
    }

    public Long getCreatedBy() {
        return createdBy;
    }

    public void setCreatedBy(Long createdBy) {
        this.createdBy = createdBy;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public List<ReportUnitVO> getReportUnitList() {
        return reportUnitList;
    }

    public void setReportUnitList(List<ReportUnitVO> reportUnitList) {
        this.reportUnitList = reportUnitList;
    }

    public List<UserDTO> getCcList() {
        return ccList;
    }

    public void setCcList(List<UserDTO> ccList) {
        this.ccList = ccList;
    }

    public List<UserDTO> getReceiverList() {
        return receiverList;
    }

    public void setReceiverList(List<UserDTO> receiverList) {
        this.receiverList = receiverList;
    }

    public String getHtml() {
        return html;
    }

    public void setHtml(String html) {
        this.html = html;
    }
}
