package io.choerodon.agile.api.vo.business;


import javax.persistence.GeneratedValue;
import javax.persistence.Id;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * 工时日志附件VO
 * @author gaokuo.dai@zknow.com 2022-12-26
 */
@ApiModel(value = "工时日志附件VO")
@JsonInclude(JsonInclude.Include.NON_NULL)
public class WorkHoursAttachmentVO {
    @Id
    @GeneratedValue
    @Encrypt
    @ApiModelProperty(value = "id")
    private Long id;

    @ApiModelProperty(value = "工时Id")
    @Encrypt
    private Long workLogId;

    @ApiModelProperty(value = "附件类型（链接或者文件）")
    private String attachmentType;

    @ApiModelProperty(value = "文件地址或者链接url")
    private String attachmentUrl;

    @ApiModelProperty(value = "文件或者链接名字")
    private String attachmentName;

    /**
     * @return id
     */
    public Long getId() {
        return id;
    }

    public WorkHoursAttachmentVO setId(Long id) {
        this.id = id;
        return this;
    }

    /**
     * @return 工时Id
     */
    public Long getWorkLogId() {
        return workLogId;
    }

    public WorkHoursAttachmentVO setWorkLogId(Long workLogId) {
        this.workLogId = workLogId;
        return this;
    }

    /**
     * @return 附件类型（链接或者文件）
     */
    public String getAttachmentType() {
        return attachmentType;
    }

    public WorkHoursAttachmentVO setAttachmentType(String attachmentType) {
        this.attachmentType = attachmentType;
        return this;
    }

    /**
     * @return 文件地址或者链接url
     */
    public String getAttachmentUrl() {
        return attachmentUrl;
    }

    public WorkHoursAttachmentVO setAttachmentUrl(String attachmentUrl) {
        this.attachmentUrl = attachmentUrl;
        return this;
    }

    /**
     * @return 文件或者链接名字
     */
    public String getAttachmentName() {
        return attachmentName;
    }

    public WorkHoursAttachmentVO setAttachmentName(String attachmentName) {
        this.attachmentName = attachmentName;
        return this;
    }
}
