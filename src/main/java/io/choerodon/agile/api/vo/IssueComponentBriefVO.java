package io.choerodon.agile.api.vo;


import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import io.choerodon.agile.infra.utils.StringUtil;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/11/7
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class IssueComponentBriefVO implements Serializable {

    @ApiModelProperty(value = "模块主键id")
    @Encrypt
    private Long componentId;

    @ApiModelProperty(value = "模块名称")
    private String name;

    @Encrypt
    @ApiModelProperty(value = "问题id")
    private Long issueId;


    public Long getComponentId() {
        return componentId;
    }

    public void setComponentId(Long componentId) {
        this.componentId = componentId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }
}
