package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.List;
import java.util.Objects;

/**
 * @author superlee
 * @since 2021-03-10
 */
public class VersionTreeVO {

    @Encrypt
    @NotNull(message = "error.version.tree.id.null")
    private Long id;

    private String name;

    @NotEmpty(message = "error.version.tree.type.empty")
    private String type;

    private String version;

    private String versionAlias;

    private List<VersionTreeVO> children;

    public String getVersionAlias() {
        return versionAlias;
    }

    public void setVersionAlias(String versionAlias) {
        this.versionAlias = versionAlias;
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

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public List<VersionTreeVO> getChildren() {
        return children;
    }

    public void setChildren(List<VersionTreeVO> children) {
        this.children = children;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof VersionTreeVO)) return false;
        VersionTreeVO that = (VersionTreeVO) o;
        return getId().equals(that.getId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getId());
    }
}
