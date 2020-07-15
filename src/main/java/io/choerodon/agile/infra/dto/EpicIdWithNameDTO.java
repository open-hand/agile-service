package io.choerodon.agile.infra.dto;


import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/7/6.
 * Email: fuqianghuang01@gmail.com
 */
public class EpicIdWithNameDTO {
    @Encrypt
    private Long epicId;

    private String epicName;

    public void setEpicId(Long epicId) {
        this.epicId = epicId;
    }

    public Long getEpicId() {
        return epicId;
    }

    public void setEpicName(String epicName) {
        this.epicName = epicName;
    }

    public String getEpicName() {
        return epicName;
    }
}
