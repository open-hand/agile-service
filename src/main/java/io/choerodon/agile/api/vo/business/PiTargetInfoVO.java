package io.choerodon.agile.api.vo.business;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2020-11-27 9:18
 */
public class PiTargetInfoVO {
    @Encrypt
    private Long id;

    private String name;

    private Long planBv;

    private Long actualBv;

    @Encrypt
    private Long piId;

    private Boolean stretch;

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
