package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 11/25/21
 */
public class BoardQuickFilterRelVO {

    @Encrypt
    @ApiModelProperty("id")
    private Long id;
    @Encrypt
    @ApiModelProperty("看版id")
    private Long boardId;
    @Encrypt
    @ApiModelProperty("快查id")
    private Long quickFilterId;
    @ApiModelProperty("项目id")
    private Long projectId;
    @ApiModelProperty("组织id")
    private Long organizationId;
    @ApiModelProperty("乐观锁")
    private Long objectVersionNumber;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getBoardId() {
        return boardId;
    }

    public void setBoardId(Long boardId) {
        this.boardId = boardId;
    }

    public Long getQuickFilterId() {
        return quickFilterId;
    }

    public void setQuickFilterId(Long quickFilterId) {
        this.quickFilterId = quickFilterId;
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

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }
}
