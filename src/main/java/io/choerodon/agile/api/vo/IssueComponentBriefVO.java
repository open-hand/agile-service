package io.choerodon.agile.api.vo;


import io.choerodon.agile.infra.utils.StringUtil;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.io.Serializable;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/11/7
 */
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
