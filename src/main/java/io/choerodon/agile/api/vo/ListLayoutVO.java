package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-05-07 14:11
 */
public class ListLayoutVO {
    @Encrypt
    private Long id;

    @NotNull(message = "error.layout.applyType.null")
    private String applyType;

    @Encrypt
    private Long userId;

    private Long projectId;

    private Long organizationId;

    private List<ListLayoutColumnRelVO> listLayoutColumnRelVOS;

    private Long objectVersionNumber;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getApplyType() {
        return applyType;
    }

    public void setApplyType(String applyType) {
        this.applyType = applyType;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public List<ListLayoutColumnRelVO> getListLayoutColumnRelVOS() {
        return listLayoutColumnRelVOS;
    }

    public void setListLayoutColumnRelVOS(List<ListLayoutColumnRelVO> listLayoutColumnRelVOS) {
        this.listLayoutColumnRelVOS = listLayoutColumnRelVOS;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }
}
