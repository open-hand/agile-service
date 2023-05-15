package io.choerodon.agile.domain.context;

import java.util.*;
import javax.validation.constraints.NotNull;

import org.apache.commons.collections4.CollectionUtils;

import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.feign.operator.RemoteIamOperator;
import io.choerodon.agile.infra.utils.SpringBeanUtil;

import org.hzero.core.util.AssertUtils;

/**
 * 项目复制上下文
 *
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
    public static final String TABLE_FD_ISSUE_TYPE = "fd_issue_type";
    public static final String TABLE_FD_ISSUE_TYPE_EXTEND = "fd_issue_type_extend";
    public static final String TABLE_FD_OBJECT_SCHEME_FIELD = "fd_object_scheme_field";
    public static final String TABLE_FD_FIELD_OPTION = "fd_field_option";
    public static final String TABLE_FD_OBJECT_SCHEME_FIELD_EXTEND = "fd_object_scheme_field_extend";
    public static final String TABLE_FD_ISSUE_TYPE_FIELD = "fd_issue_type_field";
    public static final String TABLE_FD_FIELD_PERMISSION = "fd_field_permission";
    public static final String TABLE_FD_FIELD_CASCADE_RULE = "fd_field_cascade_rule";
    public static final String TABLE_FD_FIELD_CASCADE_RULE_OPTION = "fd_field_cascade_rule_option";
    public static final String TABLE_FD_PROJECT_CONFIG = "fd_project_config";
    public static final String TABLE_FD_STATE_MACHINE_SCHEME = "fd_state_machine_scheme";
    public static final String TABLE_FD_STATUS_MACHINE = "fd_status_machine";
    public static final String TABLE_FD_STATUS_MACHINE_NODE = "fd_status_machine_node";
    public static final String TABLE_FD_ISSUE_TYPE_SCHEME = "fd_issue_type_scheme";
    public static final String TABLE_FD_ISSUE_TYPE_SCHEME_CONFIG = "fd_issue_type_scheme_config";
    public static final String TABLE_WF_DELIVERABLE = "wf_deliverable";
    public static final String TABLE_FD_STATUS_BRANCH_MERGE_SETTING = "fd_status_branch_merge_setting";
    public static final String TABLE_FD_STATUS_TRANSFER_SETTING = "fd_status_transfer_setting";
    public static final String TABLE_FD_STATUS_LINKAGE = "fd_status_linkage";
    public static final String TABLE_FD_LINK_ISSUE_STATUS_LINKAGE = "fd_link_issue_status_linkage";
    public static final String TABLE_FD_PRED_ISSUE_STATUS_LINKAGE = "fd_pred_issue_status_linkage";

    public static final String SOURCE_PROJECT = "source_project";
    public static final String TARGET_PROJECT = "target_project";

    private final Map<String, Map<Long, Long>> sourceTargetMapping = new HashMap<>();

    /**
     * 组织ID
     */
    private Long organizationId;

    /**
     * 项目类型
     */
    private Set<String> categoryCodes = new HashSet<>();

    /**
     * 复制时也许会用上的字段, 包含 -系统字段 - 当前组织的自定义字段 - 当前项目的自定义字段
     */
    private final List<ObjectSchemeFieldDTO> mayBeUsedFields = new ArrayList<>();

    private ProjectVO sourceProject;

    private ProjectVO targetProject;

    public ProjectCloneContext put(String table, Long sourceId, Long targetId) {
        this.sourceTargetMapping.computeIfAbsent(table, k -> new HashMap<>()).put(sourceId, targetId);
        return this;
    }

    public Map<Long, Long> getByTable(String table) {
        return this.sourceTargetMapping.get(table);
    }

    public Long getByTableAndSourceId(String table, Long sourceId) {
        return this.sourceTargetMapping.getOrDefault(table, Collections.emptyMap()).get(sourceId);
    }

    /**
     * @return 组织ID
     */
    public Long getOrganizationId() {
        return organizationId;
    }

    public ProjectCloneContext setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
        return this;
    }

    /**
     * @return 项目类型
     */
    public @NotNull Set<String> getCategoryCodes() {
        return this.categoryCodes;
    }

    public ProjectCloneContext setCategoryCodes(Set<String> categoryCodes) {
        if(CollectionUtils.isEmpty(categoryCodes)) {
            this.categoryCodes = new HashSet<>();
        } else {
            this.categoryCodes = categoryCodes;
        }
        return this;
    }

    /**
     * @return 复制时也许会用上的字段, 包含 -系统字段 - 当前组织的自定义字段 - 当前项目的自定义字段
     */
    public List<ObjectSchemeFieldDTO> getMayBeUsedFields() {
        return this.mayBeUsedFields;
    }

    public List<ObjectSchemeFieldDTO> addMayBeUsedFields(List<? extends ObjectSchemeFieldDTO> fields) {
        if(CollectionUtils.isEmpty(fields)) {
            return this.mayBeUsedFields;
        }
        this.mayBeUsedFields.addAll(fields);
        return mayBeUsedFields;
    }

    public ProjectVO queryProject(Long projectId, String sourceProject) {
        ProjectVO projectVO = null;
        if (SOURCE_PROJECT.equals(sourceProject)) {
            projectVO = this.sourceProject;
            if (projectVO == null) {
                RemoteIamOperator remoteIamOperator = SpringBeanUtil.getBeansOfSuper(RemoteIamOperator.class);
                projectVO = remoteIamOperator.queryProject(projectId);
                this.sourceProject = projectVO;
            }
        } else if (TARGET_PROJECT.equals(sourceProject)) {
            projectVO = this.targetProject;
            if (projectVO == null) {
                RemoteIamOperator remoteIamOperator = SpringBeanUtil.getBeansOfSuper(RemoteIamOperator.class);
                projectVO = remoteIamOperator.queryProject(projectId);
                this.targetProject = projectVO;
            }
        }
        AssertUtils.notNull(projectVO, "error.project.not.exist");
        return projectVO;
    }
}
