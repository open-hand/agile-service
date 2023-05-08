package io.choerodon.agile.domain.service.impl;

import static io.choerodon.agile.domain.context.ProjectCloneContext.*;

import java.util.*;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.choerodon.agile.app.service.AgileWaterfallService;
import io.choerodon.agile.app.service.FilePathService;
import io.choerodon.agile.domain.context.ProjectCloneContext;
import io.choerodon.agile.domain.repository.IssueRepository;
import io.choerodon.agile.domain.service.ProjectCloneDomainService;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.FieldType;
import io.choerodon.agile.infra.enums.ObjectSchemeCode;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.feign.operator.CustomFileOperator;
import io.choerodon.agile.infra.feign.vo.FileVO;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;

import org.hzero.boot.file.FileClient;

/**
 * 项目复制 领域Service Impl
 *
 * @author gaokuo.dai@zknow.com 2023-05-06
 * @since 2.5
 */
@Service
public class ProjectCloneDomainServiceImpl implements ProjectCloneDomainService {

    @Autowired
    private IssueRepository issueRepository;
    @Autowired
    private ProductVersionMapper productVersionMapper;
    @Autowired
    private VersionIssueRelMapper versionIssueRelMapper;
    @Autowired
    private IssueComponentMapper issueComponentMapper;
    @Autowired
    private ComponentIssueRelMapper componentIssueRelMapper;
    @Autowired
    private IssueLabelMapper issueLabelMapper;
    @Autowired
    private LabelIssueRelMapper labelIssueRelMapper;
    @Autowired
    private IssueLinkTypeMapper issueLinkTypeMapper;
    @Autowired
    private IssueLinkMapper issueLinkMapper;
    @Autowired
    private IssuePredecessorMapper issuePredecessorMapper;
    @Autowired
    private IssuePredecessorTreeClosureMapper issuePredecessorTreeClosureMapper;
    @Autowired
    private IssueAttachmentMapper issueAttachmentMapper;
    @Autowired
    private FilePathService filePathService;
    @Autowired
    private CustomFileOperator customFileOperator;
    @Autowired
    private FileClient fileClient;
    @Autowired(required = false)
    private AgileWaterfallService agileWaterfallService;
    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;
    @Autowired
    private ObjectSchemeFieldExtendMapper objectSchemeFieldExtendMapper;
    @Autowired
    private FieldOptionMapper fieldOptionMapper;
    @Autowired
    private FieldValueMapper fieldValueMapper;

    private final Logger logger = LoggerFactory.getLogger(ProjectCloneDomainServiceImpl.class);

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void cloneProject(Long sourceProjectId, Long targetProjectId, ProjectCloneContext context) {
        if (sourceProjectId == null || targetProjectId == null) {
            return;
        }
        if (context == null) {
            context = new ProjectCloneContext();
        }
        final Set<String> categoryCodes = context.getCategoryCodes();
        // 复制规划的版本
        cloneAgileProductVersion(sourceProjectId, targetProjectId, context);
        // 复制模块
        cloneAgileIssueComponent(sourceProjectId, targetProjectId, context);
        // 复制标签
        cloneAgileIssueLabel(sourceProjectId, targetProjectId, context);
        // 复制问题链接类型
        cloneAgileIssueLinkType(sourceProjectId, targetProjectId, context);
        //复制自定义字段
        copyCustomField(sourceProjectId, targetProjectId, context);

        // 复制issue本体
        cloneAgileIssue(sourceProjectId, targetProjectId, context);
        // 复制规划的版本与issue的关系
        cloneAgileVersionIssueRel(sourceProjectId, targetProjectId, context);
        // 复制模块与issue的关系
        cloneAgileComponentIssueRel(sourceProjectId, targetProjectId, context);
        // 复制标签与issue的关系
        cloneAgileLabelIssueRel(sourceProjectId, targetProjectId, context);
        // 复制关联工作项
        cloneAgileIssueLink(sourceProjectId, targetProjectId, context);
        // 复制工作项依赖关系
        cloneAgileIssuePredecessor(sourceProjectId, targetProjectId, context);
        // 复制工作项依赖关系树
        cloneAgileIssuePredecessorTreeClosure(sourceProjectId, targetProjectId, context);
        // 复制工作项附件
        cloneIssueAttachment(sourceProjectId, targetProjectId, context);
        //自定义字段值
        copyCustomFieldValue(sourceProjectId, targetProjectId, context);
        // TODO
        //产品暂不支持
        //tag暂不支持
        //评论暂不支持
        //ui&ux文件暂不支持

        // 复制瀑布插件数据
        if (categoryCodes.contains(ProjectCategory.MODULE_WATERFALL)) {
            if (agileWaterfallService != null) {
                agileWaterfallService.cloneProject(sourceProjectId, targetProjectId, context);
            }
        }
    }

    private void copyCustomFieldValue(Long sourceProjectId,
                                      Long targetProjectId,
                                      ProjectCloneContext context) {
        List<FieldValueDTO> fieldValues = fieldValueMapper.select(new FieldValueDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(fieldValues)) {
            this.logger.debug("没有检测到可复制的 fd_field_value 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_field_value 数据{}条, 开始复制", fieldValues.size());
        for (FieldValueDTO fieldValue : fieldValues) {
            fieldValue.setId(null);
            fieldValue.setProjectId(targetProjectId);
            String schemeCode = fieldValue.getSchemeCode();
            Long sourceInstanceId = fieldValue.getInstanceId();
            Long targetInstanceId = null;
            if (ObjectSchemeCode.AGILE_ISSUE.equals(schemeCode)) {
                targetInstanceId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceInstanceId);
            } else if (ObjectSchemeCode.BACKLOG.equals(schemeCode)) {
            }
            if (targetInstanceId == null) {
                continue;
            }
            fieldValue.setInstanceId(targetInstanceId);
            Long sourceFieldId = fieldValue.getFieldId();
            Long targetFieldId = context.getByTableAndSourceId(TABLE_FD_OBJECT_SCHEME_FIELD, sourceFieldId);
            if (targetFieldId == null) {
                continue;
            }
            String fieldType = context.getFieldType(sourceFieldId);
            Long sourceOptionId = fieldValue.getOptionId();
            if (sourceOptionId != null) {
                if (FieldType.MEMBER.equals(fieldType) || FieldType.MULTI_MEMBER.equals(fieldType)) {
                    Long currentUserId = DetailsHelper.getUserDetails().getUserId();
                    if (!Objects.equals(sourceOptionId, currentUserId)) {
                        //不是操作人，跳过
                        continue;
                    }
                } else {
                    Long targetOptionId = context.getByTableAndSourceId(TABLE_FD_FIELD_OPTION, sourceOptionId);
                    if (targetOptionId == null) {
                        continue;
                    }
                    fieldValue.setOptionValue(targetOptionId.toString());
                }
            }
            if (fieldValueMapper.insert(fieldValue) != 1) {
                throw new CommonException("error.insert.fd_field_value");
            }
        }
        this.logger.debug("fd_field_value 复制完成");
    }

    private void copyCustomField(Long sourceProjectId,
                                 Long targetProjectId,
                                 ProjectCloneContext context) {
        cloneFdObjectSchemeField(sourceProjectId, targetProjectId, context);
        copyFdObjectSchemeFieldExtend(sourceProjectId, targetProjectId, context);
        copyFdFieldOption(sourceProjectId, targetProjectId, context);
    }

    private void copyFdFieldOption(Long sourceProjectId,
                                   Long targetProjectId,
                                   ProjectCloneContext context) {
        Set<Long> fieldIds =
                Optional.ofNullable(context.getByTable(TABLE_FD_OBJECT_SCHEME_FIELD)).orElse(Collections.emptyMap()).keySet();
        if (CollectionUtils.isEmpty(fieldIds)) {
            return;
        }
        Long organizationId = ConvertUtil.getOrganizationId(sourceProjectId);
        List<FieldOptionDTO> fieldOptions = fieldOptionMapper.selectByFieldIds(organizationId, new ArrayList<>(fieldIds));
        if (CollectionUtils.isEmpty(fieldOptions)) {
            this.logger.debug("没有检测到可复制的 fd_field_option 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_field_option 数据{}条, 开始复制", fieldOptions.size());
        for (FieldOptionDTO fieldOption : fieldOptions) {
            Long sourceId = fieldOption.getId();
            fieldOption.setId(null);
            Long sourceFieldId = fieldOption.getFieldId();
            Long targetFieldId = context.getByTableAndSourceId(TABLE_FD_OBJECT_SCHEME_FIELD, sourceFieldId);
            if (targetFieldId == null) {
                continue;
            }
            fieldOption.setFieldId(targetFieldId);
            if (fieldOptionMapper.insert(fieldOption) != 1) {
                throw new CommonException("error.insert.fd_field_option");
            }
            context.put(TABLE_FD_FIELD_OPTION, sourceId, fieldOption.getId());
        }
        this.logger.debug("fd_field_option 复制完成");
    }

    private void copyFdObjectSchemeFieldExtend(Long sourceProjectId,
                                               Long targetProjectId,
                                               ProjectCloneContext context) {
        List<ObjectSchemeFieldExtendDTO> sourceExtendList = objectSchemeFieldExtendMapper.select(new ObjectSchemeFieldExtendDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceExtendList)) {
            this.logger.debug("没有检测到可复制的 fd_object_scheme_field_extend 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_object_scheme_field_extend 数据{}条, 开始复制", sourceExtendList.size());
        for (ObjectSchemeFieldExtendDTO extend : sourceExtendList) {
            extend.setId(null);
            extend.setProjectId(targetProjectId);
            Long sourceIssueTypeId = extend.getIssueTypeId();
            Long targetIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, sourceIssueTypeId);
            if (targetIssueTypeId != null) {
                extend.setIssueTypeId(targetIssueTypeId);
            }
            Long sourceFieldId = extend.getFieldId();
            Long targetFieldId = context.getByTableAndSourceId(TABLE_FD_OBJECT_SCHEME_FIELD, sourceFieldId);
            if (targetFieldId != null) {
                extend.setFieldId(targetFieldId);
            }
            if (objectSchemeFieldExtendMapper.insert(extend) != 1) {
                throw new CommonException("error.insert.fd_object_scheme_field_extend");
            }
        }
        this.logger.debug("fd_object_scheme_field_extend 复制完成");
    }

    private void cloneFdObjectSchemeField(Long sourceProjectId,
                                          Long targetProjectId,
                                          ProjectCloneContext context) {
        List<ObjectSchemeFieldDTO> sourceObjectSchemeFields = this.objectSchemeFieldMapper.select(new ObjectSchemeFieldDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceObjectSchemeFields)) {
            this.logger.debug("没有检测到可复制的 fd_object_scheme_field 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_object_scheme_field 数据{}条, 开始复制", sourceObjectSchemeFields.size());
        for (ObjectSchemeFieldDTO field : sourceObjectSchemeFields) {
            Long sourceFieldId = field.getId();
            field.setId(null);
            field.setProjectId(targetProjectId);
            context.putFieldType(sourceFieldId, field.getFieldType());
            if (objectSchemeFieldMapper.insert(field) != 1) {
                throw new CommonException("error.insert.fd_object_scheme_field");
            }
            Long targetFieldId = field.getId();
            context.put(TABLE_FD_OBJECT_SCHEME_FIELD, sourceFieldId, targetFieldId);
        }
        this.logger.debug("fd_object_scheme_field 复制完成");
    }

    /**
     * 复制issue表
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileIssue(Long sourceProjectId,
                                 Long targetProjectId,
                                 ProjectCloneContext context) {
        List<IssueDTO> sourceIssues = this.issueRepository.select(new IssueDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssues)) {
            this.logger.debug("没有检测到可复制的 agile_issue 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue 数据{}条, 开始复制", sourceIssues.size());
        for (IssueDTO issue : sourceIssues) {
            Long sourceIssueId = issue.getIssueId();
            issue.setProjectId(targetProjectId);
            issue.setIssueId(null);
            // TODO 设置问题类型ID
            // TODO 设置项目状态ID
            this.issueRepository.insert(issue);
            Long targetIssueId = issue.getIssueId();
            context.put(TABLE_AGILE_ISSUE, sourceIssueId, targetIssueId);
        }
        this.logger.debug("agile_issue 复制完成");
    }

    /**
     * 复制规划的版本 agile_product_version
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileProductVersion(Long sourceProjectId,
                                          Long targetProjectId,
                                          ProjectCloneContext context) {
        List<ProductVersionDTO> sourceProductVersions = productVersionMapper.select(new ProductVersionDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceProductVersions)) {
            this.logger.debug("没有检测到可复制的 agile_product_version 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_product_version 数据{}条, 开始复制", sourceProductVersions.size());
        for (ProductVersionDTO productVersionDTO : sourceProductVersions) {
            Long sourceVersionId = productVersionDTO.getVersionId();
            productVersionDTO.setVersionId(null);
            productVersionDTO.setProjectId(targetProjectId);
            if (productVersionMapper.insert(productVersionDTO) != 1) {
                throw new CommonException("error.insert.product_version");
            }
            context.put(TABLE_AGILE_PRODUCT_VERSION, sourceVersionId, productVersionDTO.getVersionId());
        }
        this.logger.debug("agile_product_version 复制完成");
    }

    /**
     * 复制规划的版本与issue的关系 agile_version_issue_rel
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileVersionIssueRel(Long sourceProjectId,
                                           Long targetProjectId,
                                           ProjectCloneContext context) {
        List<VersionIssueRelDTO> sourceVersionIssueRelList = versionIssueRelMapper.select(new VersionIssueRelDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceVersionIssueRelList)) {
            this.logger.debug("没有检测到可复制的 agile_version_issue_rel 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_version_issue_rel 数据{}条, 开始复制", sourceVersionIssueRelList.size());
        for (VersionIssueRelDTO versionIssueRelDTO : sourceVersionIssueRelList) {
            versionIssueRelDTO.setId(null);
            versionIssueRelDTO.setProjectId(targetProjectId);
            Long sourceVersionId = versionIssueRelDTO.getVersionId();
            Long targetVersionId = context.getByTableAndSourceId(TABLE_AGILE_PRODUCT_VERSION, sourceVersionId);
            if (targetVersionId == null) {
                continue;
            }
            versionIssueRelDTO.setVersionId(targetVersionId);
            Long sourceIssueId = versionIssueRelDTO.getIssueId();
            Long targetIssueId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceIssueId);
            if (targetIssueId == null) {
                continue;
            }
            versionIssueRelDTO.setIssueId(targetIssueId);
            if (versionIssueRelMapper.insert(versionIssueRelDTO) != 1) {
                throw new CommonException("error.insert.version_issue_rel");
            }
        }
        this.logger.debug("agile_version_issue_rel 复制完成");
    }

    /**
     * 复制模块 agile_issue_component
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileIssueComponent(Long sourceProjectId,
                                          Long targetProjectId,
                                          ProjectCloneContext context) {
        List<IssueComponentDTO> sourceIssueComponentList = issueComponentMapper.select(new IssueComponentDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssueComponentList)) {
            this.logger.debug("没有检测到可复制的 agile_issue_component 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue_component 数据{}条, 开始复制", sourceIssueComponentList.size());
        for (IssueComponentDTO issueComponentDTO : sourceIssueComponentList) {
            Long componentId = issueComponentDTO.getComponentId();
            issueComponentDTO.setComponentId(null);
            issueComponentDTO.setProjectId(targetProjectId);
            // TODO 处理模块负责人
            if (issueComponentMapper.insert(issueComponentDTO) != 1) {
                throw new CommonException("error.insert.agile_issue_component");
            }
            context.put(TABLE_AGILE_ISSUE_COMPONENT, componentId, issueComponentDTO.getComponentId());
        }
        this.logger.debug("agile_issue_component 复制完成");
    }

    /**
     * 复制模块与issue的关系 agile_component_issue_rel
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileComponentIssueRel(Long sourceProjectId,
                                             Long targetProjectId,
                                             ProjectCloneContext context) {
        List<ComponentIssueRelDTO> sourceComponentIssueRelList = componentIssueRelMapper.select(new ComponentIssueRelDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceComponentIssueRelList)) {
            this.logger.debug("没有检测到可复制的 agile_component_issue_rel 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_component_issue_rel 数据{}条, 开始复制", sourceComponentIssueRelList.size());
        for (ComponentIssueRelDTO componentIssueRelDTO : sourceComponentIssueRelList) {
            componentIssueRelDTO.setId(null);
            componentIssueRelDTO.setProjectId(targetProjectId);
            Long sourceComponentId = componentIssueRelDTO.getComponentId();
            Long targetComponentId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE_COMPONENT, sourceComponentId);
            if (targetComponentId == null) {
                continue;
            }
            componentIssueRelDTO.setComponentId(targetComponentId);
            Long sourceIssueId = componentIssueRelDTO.getIssueId();
            Long targetIssueId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceIssueId);
            if (targetIssueId == null) {
                continue;
            }
            componentIssueRelDTO.setIssueId(targetIssueId);
            if (componentIssueRelMapper.insert(componentIssueRelDTO) != 1) {
                throw new CommonException("error.insert.agile_component_issue_rel");
            }
        }
        this.logger.debug("agile_component_issue_rel 复制完成");
    }

    /**
     * 复制标签 agile_issue_label
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileIssueLabel(Long sourceProjectId,
                                      Long targetProjectId,
                                      ProjectCloneContext context) {
        List<IssueLabelDTO> sourceIssueLabelList = issueLabelMapper.select(new IssueLabelDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssueLabelList)) {
            this.logger.debug("没有检测到可复制的 agile_issue_label 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue_label 数据{}条, 开始复制", sourceIssueLabelList.size());
        for (IssueLabelDTO issueLabelDTO : sourceIssueLabelList) {
            Long labelId = issueLabelDTO.getLabelId();
            issueLabelDTO.setLabelId(null);
            issueLabelDTO.setProjectId(targetProjectId);
            if (issueLabelMapper.insert(issueLabelDTO) != 1) {
                throw new CommonException("error.insert.agile_issue_label");
            }
            context.put(TABLE_AGILE_ISSUE_LABEL, labelId, issueLabelDTO.getLabelId());
        }
        this.logger.debug("agile_issue_label 复制完成");
    }

    /**
     * 复制标签与issue的关系 agile_label_issue_rel
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileLabelIssueRel(Long sourceProjectId,
                                         Long targetProjectId,
                                         ProjectCloneContext context) {
        List<LabelIssueRelDTO> sourceLabelIssueRelList = labelIssueRelMapper.select(new LabelIssueRelDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceLabelIssueRelList)) {
            this.logger.debug("没有检测到可复制的 agile_label_issue_rel 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_label_issue_rel 数据{}条, 开始复制", sourceLabelIssueRelList.size());
        for (LabelIssueRelDTO labelIssueRelDTO : sourceLabelIssueRelList) {
            labelIssueRelDTO.setProjectId(targetProjectId);
            Long sourceLabelId = labelIssueRelDTO.getLabelId();
            Long targetLabelId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE_LABEL, sourceLabelId);
            if (targetLabelId == null) {
                continue;
            }
            labelIssueRelDTO.setLabelId(targetLabelId);
            Long sourceIssueId = labelIssueRelDTO.getIssueId();
            Long targetIssueId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceIssueId);
            if (targetIssueId == null) {
                continue;
            }
            labelIssueRelDTO.setIssueId(targetIssueId);
            if (labelIssueRelMapper.insert(labelIssueRelDTO) != 1) {
                throw new CommonException("error.insert.agile_label_issue_rel");
            }
        }
        this.logger.debug("agile_label_issue_rel 复制完成");
    }

    /**
     * 复制问题链接类型 agile_issue_link_type
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileIssueLinkType(Long sourceProjectId,
                                         Long targetProjectId,
                                         ProjectCloneContext context) {
        List<IssueLinkTypeDTO> sourceIssueLinkTypeList = issueLinkTypeMapper.select(new IssueLinkTypeDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssueLinkTypeList)) {
            this.logger.debug("没有检测到可复制的 agile_issue_link_type 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue_link_type 数据{}条, 开始复制", sourceIssueLinkTypeList.size());
        for (IssueLinkTypeDTO issueLinkTypeDTO : sourceIssueLinkTypeList) {
            Long sourceLinkTypeId = issueLinkTypeDTO.getLinkTypeId();
            issueLinkTypeDTO.setLinkTypeId(null);
            issueLinkTypeDTO.setProjectId(targetProjectId);
            if (issueLinkTypeMapper.insert(issueLinkTypeDTO) != 1) {
                throw new CommonException("error.insert.agile_issue_link_type");
            }
            context.put(TABLE_AGILE_ISSUE_LINK_TYPE, sourceLinkTypeId, issueLinkTypeDTO.getLinkTypeId());
        }
        this.logger.debug("agile_issue_link_type 复制完成");
    }

    /**
     * 复制关联工作项 agile_issue_link
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileIssueLink(Long sourceProjectId,
                                     Long targetProjectId,
                                     ProjectCloneContext context) {
        List<IssueLinkDTO> sourceIssueLinkList = issueLinkMapper.select(new IssueLinkDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssueLinkList)) {
            this.logger.debug("没有检测到可复制的 agile_issue_link 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue_link 数据{}条, 开始复制", sourceIssueLinkList.size());
        for (IssueLinkDTO issueLinkDTO : sourceIssueLinkList) {
            issueLinkDTO.setLinkId(null);
            issueLinkDTO.setProjectId(targetProjectId);
            Long sourceIssueId = issueLinkDTO.getIssueId();
            Long targetIssueId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceIssueId);
            if (targetIssueId == null) {
                continue;
            }
            issueLinkDTO.setIssueId(targetIssueId);
            Long sourceLinkedIssueId = issueLinkDTO.getLinkedIssueId();
            Long targetLinkedIssueId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceLinkedIssueId);
            if (targetLinkedIssueId == null) {
                continue;
            }
            issueLinkDTO.setLinkedIssueId(targetLinkedIssueId);
            Long sourceLinkTypeId = issueLinkDTO.getLinkTypeId();
            Long targetLinkTypeId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE_LINK_TYPE, sourceLinkTypeId);
            if (targetLinkTypeId == null) {
                continue;
            }
            issueLinkDTO.setLinkTypeId(targetLinkTypeId);
            if (issueLinkMapper.insert(issueLinkDTO) != 1) {
                throw new CommonException("error.insert.agile_issue_link");
            }
        }
        this.logger.debug("agile_issue_link 复制完成");
    }

    /**
     * 复制工作项依赖关系 agile_issue_predecessor
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileIssuePredecessor(Long sourceProjectId,
                                            Long targetProjectId,
                                            ProjectCloneContext context) {
        List<IssuePredecessorDTO> sourceIssuePredecessorList = issuePredecessorMapper.select(new IssuePredecessorDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssuePredecessorList)) {
            this.logger.debug("没有检测到可复制的 agile_issue_predecessor 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue_predecessor 数据{}条, 开始复制", sourceIssuePredecessorList.size());
        for (IssuePredecessorDTO issuePredecessorDTO : sourceIssuePredecessorList) {
            issuePredecessorDTO.setId(null);
            issuePredecessorDTO.setProjectId(targetProjectId);
            Long sourceIssueId = issuePredecessorDTO.getIssueId();
            Long targetIssueId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceIssueId);
            if (targetIssueId == null) {
                continue;
            }
            issuePredecessorDTO.setIssueId(targetIssueId);
            Long sourcePredecessorId = issuePredecessorDTO.getPredecessorId();
            Long targetPredecessorId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourcePredecessorId);
            if (targetPredecessorId == null) {
                continue;
            }
            issuePredecessorDTO.setPredecessorId(targetPredecessorId);
            if (issuePredecessorMapper.insert(issuePredecessorDTO) != 1) {
                throw new CommonException("error.insert.agile_issue_predecessor");
            }
        }
        this.logger.debug("agile_issue_predecessor 复制完成");
    }

    /**
     * 复制工作项依赖关系树 agile_issue_predecessor_tree_closure
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileIssuePredecessorTreeClosure(Long sourceProjectId,
                                                       Long targetProjectId,
                                                       ProjectCloneContext context) {
        List<IssuePredecessorTreeClosureDTO> sourceIssuePredecessorTreeClosureList = issuePredecessorTreeClosureMapper.select(new IssuePredecessorTreeClosureDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssuePredecessorTreeClosureList)) {
            this.logger.debug("没有检测到可复制的 agile_issue_predecessor_tree_closure 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue_predecessor_tree_closure 数据{}条, 开始复制", sourceIssuePredecessorTreeClosureList.size());
        for (IssuePredecessorTreeClosureDTO issuePredecessorTreeClosureDTO : sourceIssuePredecessorTreeClosureList) {
            issuePredecessorTreeClosureDTO.setId(null);
            issuePredecessorTreeClosureDTO.setProjectId(targetProjectId);
            Long sourceAncestorId = issuePredecessorTreeClosureDTO.getAncestorId();
            Long targetAncestorId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceAncestorId);
            if (targetAncestorId == null) {
                continue;
            }
            issuePredecessorTreeClosureDTO.setAncestorId(targetAncestorId);
            Long sourceDescendantId = issuePredecessorTreeClosureDTO.getDescendantId();
            Long targetDescendantId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceDescendantId);
            if (targetDescendantId == null) {
                continue;
            }
            issuePredecessorTreeClosureDTO.setDescendantId(targetDescendantId);
            if (issuePredecessorTreeClosureMapper.insert(issuePredecessorTreeClosureDTO) != 1) {
                throw new CommonException("error.insert.agile_issue_predecessor_tree_closure");
            }
        }
        this.logger.debug("agile_issue_predecessor_tree_closure 复制完成");
    }

    /**
     * 复制工作项附件 agile_issue_attachment & OSS
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneIssueAttachment(Long sourceProjectId,
                                      Long targetProjectId,
                                      ProjectCloneContext context) {
        List<IssueAttachmentDTO> sourceIssueAttachmentList = issueAttachmentMapper.select(new IssueAttachmentDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssueAttachmentList)) {
            this.logger.debug("没有检测到可复制的 agile_issue_attachment 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue_attachment 数据{}条, 开始复制", sourceIssueAttachmentList.size());
        Set<String> urls = new HashSet<>();
        for (IssueAttachmentDTO issueAttachmentDTO : sourceIssueAttachmentList) {
            issueAttachmentDTO.setAttachmentId(null);
            issueAttachmentDTO.setProjectId(targetProjectId);
            Long sourceIssueId = issueAttachmentDTO.getIssueId();
            Long targetIssueId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceIssueId);
            if (targetIssueId == null) {
                continue;
            }
            issueAttachmentDTO.setIssueId(targetIssueId);
            String url = issueAttachmentDTO.getUrl();
            if (url != null) {
                String fullUrl = filePathService.generateFullPath(url);
                if (url.startsWith("/")) {
                    url = url.substring(1);
                }
                issueAttachmentDTO.setUrl(fullUrl);
                urls.add(url);
            }
        }
        List<FileVO> files = new ArrayList<>();
        Long organizationId = ConvertUtil.getOrganizationId(targetProjectId);
        if (!urls.isEmpty()) {
            files.addAll(customFileOperator.getFileByFileKey(organizationId, new ArrayList<>(urls)));
        }
        Map<String, String> urlMap = new HashMap<>();
        for (FileVO file : files) {
            String cloneUrl = fileClient.copyFileByUrl(organizationId, file.getFileUrl(), file.getBucketName(), file.getBucketName(), file.getFileName());
            urlMap.put(file.getFileUrl(), cloneUrl);
        }
        for (IssueAttachmentDTO issueAttachmentDTO : sourceIssueAttachmentList) {
            String url = issueAttachmentDTO.getUrl();
            if (url != null) {
                String cloneUrl = urlMap.get(url);
                if (cloneUrl != null) {
                    issueAttachmentDTO.setUrl(filePathService.generateRelativePath(cloneUrl));
                    issueAttachmentMapper.insert(issueAttachmentDTO);
                }
            }
        }
        this.logger.debug("agile_issue_predecessor_tree_closure 复制完成");
    }

}
