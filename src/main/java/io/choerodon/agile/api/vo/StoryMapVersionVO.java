package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/6/6.
 * Email: fuqianghuang01@gmail.com
 */
public class StoryMapVersionVO {
    @ApiModelProperty(value = "版本id")
    private Long versionId;
    @ApiModelProperty(value = "名称")
    private String name;

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
}
