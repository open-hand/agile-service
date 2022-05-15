package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2020-11-27 9:18
 */
public class PiTargetInfoVO {
    @Encrypt
    @ApiModelProperty("主键")
    private Long id;
    @ApiModelProperty("名称")
    private String name;
    @ApiModelProperty("计划")
    private Long planBv;
    @ApiModelProperty("实际")
    private Long actualBv;

    @Encrypt
    @ApiModelProperty("pi id")
    private Long piId;
    @ApiModelProperty("策略")
    private Boolean stretch;
    @ApiModelProperty("项目群id")
    private Long programId;

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

    public Long getPlanBv() {
        return planBv;
    }

    public void setPlanBv(Long planBv) {
        this.planBv = planBv;
    }

    public Long getActualBv() {
        return actualBv;
    }

    public void setActualBv(Long actualBv) {
        this.actualBv = actualBv;
    }

    public Long getPiId() {
        return piId;
    }

    public void setPiId(Long piId) {
        this.piId = piId;
    }

    public Boolean getStretch() {
        return stretch;
    }

    public void setStretch(Boolean stretch) {
        this.stretch = stretch;
    }

    public Long getProgramId() {
        return programId;
    }

    public void setProgramId(Long programId) {
        this.programId = programId;
    }
}
