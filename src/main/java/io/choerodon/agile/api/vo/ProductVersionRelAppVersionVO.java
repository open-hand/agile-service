package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author chihao.ran@hand-china.com
 * 2021/03/12 9:53
 */
public class ProductVersionRelAppVersionVO {
    @ApiModelProperty(value = "应用版本id")
    @Encrypt
    private List<Long> appVersionIds;

    public List<Long> getAppVersionIds() {
        return appVersionIds;
    }

    public void setAppVersionIds(List<Long> appVersionIds) {
        this.appVersionIds = appVersionIds;
    }
}
