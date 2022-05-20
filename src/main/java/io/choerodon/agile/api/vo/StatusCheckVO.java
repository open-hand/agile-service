package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/11/30.
 * Email: fuqianghuang01@gmail.com
 */
public class StatusCheckVO {
    @ApiModelProperty(value = "状态是否存在")
    private Boolean statusExist;
    @ApiModelProperty(value = "状态id")
    @Encrypt
    private Long id;
    @ApiModelProperty(value = "名称")
    private String name;
    @ApiModelProperty(value = "状态类型（todo/doing/done/none/prepare）")
    private String type;
    @ApiModelProperty(value = "存在的问题类型")
    private List<IssueTypeVO> existIssueTypeVO;

    public void setStatusExist(Boolean statusExist) {
        this.statusExist = statusExist;
    }

    public Boolean getStatusExist() {
        return statusExist;
    }

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

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public List<IssueTypeVO> getExistIssueTypeVO() {
        return existIssueTypeVO;
    }

    public void setExistIssueTypeVO(List<IssueTypeVO> existIssueTypeVO) {
        this.existIssueTypeVO = existIssueTypeVO;
    }
}
