package io.choerodon.agile.api.vo;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.infra.utils.StringUtil;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.io.Serializable;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/7/26
 */
public class PieChartVO implements Serializable {
    @ApiModelProperty(value = "名称")
    private String name;
    @ApiModelProperty(value = "登录名")
    private String loginName;
    @ApiModelProperty(value = "真实名")
    private String realName;

    @Encrypt(ignoreValue = {"0"})
    @ApiModelProperty(value = "类型名称")
    private String typeName;
    @ApiModelProperty(value = "值")
    private Integer value;
    @ApiModelProperty(value = "百分比")
    private Double percent;
    @ApiModelProperty(value = "json数据")
    private JSONObject jsonObject;
    @ApiModelProperty(value = "优先级")
    private PriorityVO priorityVO;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Integer getValue() {
        return value;
    }

    public void setValue(Integer value) {
        this.value = value;
    }

    public Double getPercent() {
        return percent;
    }

    public void setPercent(Double percent) {
        this.percent = percent;
    }

    public String getTypeName() {
        return typeName;
    }

    public void setTypeName(String typeName) {
        this.typeName = typeName;
    }

    public JSONObject getJsonObject() {
        return jsonObject;
    }

    public void setJsonObject(JSONObject jsonObject) {
        this.jsonObject = jsonObject;
    }

    public void setPriorityVO(PriorityVO priorityVO) {
        this.priorityVO = priorityVO;
    }

    public PriorityVO getPriorityVO() {
        return priorityVO;
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

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

}
