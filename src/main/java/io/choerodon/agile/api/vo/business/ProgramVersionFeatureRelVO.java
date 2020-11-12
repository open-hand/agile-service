package io.choerodon.agile.api.vo.business;

/**
 * @author zhaotianxin
 * @date 2020-11-12 14:25
 */
public class ProgramVersionFeatureRelVO {
    private Long id;

    private Long featureId;

    private Long programVersionId;

    private String name;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getFeatureId() {
        return featureId;
    }

    public void setFeatureId(Long featureId) {
        this.featureId = featureId;
    }

    public Long getProgramVersionId() {
        return programVersionId;
    }

    public void setProgramVersionId(Long programVersionId) {
        this.programVersionId = programVersionId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
