package io.choerodon.agile.domain.context;

import java.util.*;
import javax.validation.constraints.NotNull;

/**
 * 项目复制上下文
 * @author superlee 2023/4/27
 * @since 2.5
 */
public class ProjectCloneContext {

    public static final String TABLE_AGILE_ISSUE = "agile_issue";
    public static final String TABLE_AGILE_PRODUCT_VERSION = "agile_product_version";
    public static final String TABLE_AGILE_ISSUE_COMPONENT = "agile_issue_component";
    public static final String TABLE_AGILE_ISSUE_LABEL = "agile_issue_label";
    public static final String TABLE_WF_ISSUE_EXTEND = "wf_issue_extend";
    public static final String TABLE_WF_ISSUE_SNAPSHOT = "wf_issue_snapshot";
    public static final String TABLE_WF_SNAPSHOT = "wf_snapshot";
    public static final String TABLE_AGILE_ISSUE_LINK_TYPE = "agile_issue_link_type";
    public static final String TABLE_FD_OBJECT_SCHEME_FIELD = "fd_object_scheme_field";
    public static final String TABLE_FD_ISSUE_TYPE = "fd_issue_type";
    public static final String TABLE_FD_FIELD_OPTION = "fd_field_option";

    private final Map<String, Map<Long, Long>> sourceTargetMapping = new HashMap<>();

    private final Map<Long, String> fieldIdTypeMapping = new HashMap<>();

    /**
     * 项目类型
     */
    private volatile Set<String> categoryCodes = null;

    public ProjectCloneContext put(String table, Long sourceId, Long targetId) {
        sourceTargetMapping.computeIfAbsent(table, k -> new HashMap<>()).put(sourceId, targetId);
        return this;
    }

    public Map<Long, Long> getByTable(String table) {
        return sourceTargetMapping.get(table);
    }

    public Long getByTableAndSourceId(String table, Long sourceId) {
        return sourceTargetMapping.getOrDefault(table, Collections.emptyMap()).get(sourceId);
    }

    /**
     * @return 项目类型
     */
    public @NotNull Set<String> getCategoryCodes() {
        if(this.categoryCodes == null) {
            synchronized (this) {
                if(this.categoryCodes == null) {
                    this.categoryCodes = new HashSet<>();
                }
            }
        }
        return categoryCodes;
    }

    public ProjectCloneContext setCategoryCodes(Set<String> categoryCodes) {
        this.categoryCodes = categoryCodes;
        return this;
    }

    public ProjectCloneContext putFieldType(Long sourceFieldId, String fieldType) {
        fieldIdTypeMapping.put(sourceFieldId, fieldType);
        return this;
    }

    public String getFieldType(Long sourceFieldId) {
        return fieldIdTypeMapping.get(sourceFieldId);
    }
}
