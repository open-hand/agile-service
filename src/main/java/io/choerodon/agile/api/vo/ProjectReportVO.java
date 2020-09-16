package io.choerodon.agile.api.vo;

import java.util.Date;
import java.util.List;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

import io.choerodon.agile.api.vo.report.ReportUnitVO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 下午5:53
 */
public class ProjectReportVO {
    public static final String FIELD_TITLE = "title";
    public static final String FIELD_DESCRIPTION = "description";
    public static final String FIELD_RECEIVERID = "receiverId";

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
    @ApiModelProperty("收件人Id")
    @Encrypt
    private Long receiverId;
    @ApiModelProperty("收件人")
    private UserDTO receiver;
    @ApiModelProperty("抄送人List")
    private List<UserDTO> ccList;
    @ApiModelProperty("报表数据")
    private List<ReportUnitVO> reportUnitList;
    @ApiModelProperty("最近发送时间")
    private Date recentSendDate;
    @ApiModelProperty("创建人")
    @Encrypt
    private Long createdBy;

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

    public Long getReceiverId() {
        return receiverId;
    }

    public void setReceiverId(Long receiverId) {
        this.receiverId = receiverId;
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

    public UserDTO getReceiver() {
        return receiver;
    }

    public void setReceiver(UserDTO receiver) {
        this.receiver = receiver;
    }
}
