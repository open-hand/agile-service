package io.choerodon.agile.infra.utils;

import java.lang.reflect.Field;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import javax.persistence.Id;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.infra.dto.IssueCommentDTO;
import io.choerodon.agile.infra.dto.IssueLinkDTO;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.mapper.IssueCommentMapper;
import io.choerodon.agile.infra.mapper.IssueLinkMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.core.convertor.ApplicationContextHelper;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.common.BaseMapper;
import io.choerodon.mybatis.domain.AuditDomain;

import org.hzero.core.base.BaseConstants;
import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;

/**
 * 表基础字段处理工具类
 *
 * @author jiaxu.cui@hand-china.com 2020/7/6 上午11:06
 */
public class BaseFieldUtil {

    protected BaseFieldUtil() {
    }

    private static final Logger LOGGER = LoggerFactory.getLogger(BaseFieldUtil.class);

    protected static final String FIELD_ORGANIZATION_ID = "organizationId";
    protected static final String FIELD_PROJECT_ID = "projectId";

    private static IssueMapper issueMapper;

    /**
     * 更新issue的最后更新时间，最后更新人
     *
     * @param issueId    主键值
     * @param projectId  项目id
     */
    public static IssueDTO updateIssueLastUpdateInfo(Long issueId, Long projectId) {
        if (Objects.isNull(issueId) || Objects.equals(issueId, 0L)) {
            return null;
        }
        if (Objects.isNull(issueMapper)) {
            issueMapper = ApplicationContextHelper.getContext().getBean(IssueMapper.class);
        }
        return updateLastUpdateInfo(issueMapper, IssueDTO.class, issueId, null, projectId);
    }

    /**
     * 根据评论id更新issue
     *
     * @param commentMapper commentMapper
     * @param commentId     评论id
     * @return 更新后的对象
     */
    public static IssueDTO updateIssueLastUpdateInfoByCommentId(IssueCommentMapper commentMapper, Long commentId) {
        IssueCommentDTO issueCommentDTO = commentMapper.selectByPrimaryKey(commentId);
        return BaseFieldUtil.updateIssueLastUpdateInfo(issueCommentDTO.getIssueId(), issueCommentDTO.getProjectId());
    }

    /**
     * 根据issueline更新issue，包括IssueId和LinkedIssueId
     *
     * @param projectId    项目id
     * @param issueLinkDTO issueLinkDTO
     */
    public static void updateIssueLastUpdateInfoForIssueLink(Long projectId, IssueLinkDTO issueLinkDTO) {
        if (Objects.isNull(issueLinkDTO)) {
            return;
        }
        BaseFieldUtil.updateIssueLastUpdateInfo(issueLinkDTO.getIssueId(), projectId);
        BaseFieldUtil.updateIssueLastUpdateInfo(issueLinkDTO.getLinkedIssueId(), projectId);
    }

    /**
     * 根据issueId, 更新所有关联到的issue
     *
     * @param issueLinkMapper issueLinkMapper
     * @param issueId         根据issueId
     */
    public static void updateIssueLastUpdateInfoForALLIssueLink(IssueLinkMapper issueLinkMapper, Long issueId) {
        // 更新关联的issue的最后更新时间，更新人
        List<IssueLinkDTO> issueLinkList = issueLinkMapper.selectByCondition(Condition.builder(IssueLinkDTO.class)
                .orWhere(Sqls.custom().andEqualTo(IssueLinkDTO.FIELD_ISSUE_ID, issueId)
                        .andNotEqualTo(IssueLinkDTO.FIELD_ISSUE_ID, 0L))
                .orWhere(Sqls.custom().andEqualTo(IssueLinkDTO.FIELD_LINKED_ISSUE_ID, issueId)
                        .andNotEqualTo(IssueLinkDTO.FIELD_LINKED_ISSUE_ID, 0L)).build());
        if (CollectionUtils.isEmpty(issueLinkList)) {
            return;
        }
        issueLinkList.forEach(link -> BaseFieldUtil.updateIssueLastUpdateInfoForIssueLink(link.getProjectId(), link));
    }


    /**
     * 更新最后更新时间，最后更新人
     *
     * @param mapper         {@link BaseMapper}
     * @param clazz          实例类
     * @param primaryKey     主键值
     * @param organizationId 组织id
     * @param projectId      项目id
     * @param <T>            实体类
     */
    public static <T extends AuditDomain> T updateLastUpdateInfo(BaseMapper<T> mapper,
                                                                 Class<T> clazz,
                                                                 Long primaryKey,
                                                                 Long organizationId,
                                                                 Long projectId) {
        Assert.notNull(primaryKey, BaseConstants.ErrorCode.DATA_INVALID);
        Assert.isTrue(Objects.nonNull(organizationId) || Objects.nonNull(projectId),
                BaseConstants.ErrorCode.DATA_INVALID);
        Field[] fields = FieldUtils.getFieldsWithAnnotation(clazz, Id.class);
        if (fields.length != 1) {
            throw new CommonException(BaseConstants.ErrorCode.ERROR);
        }
        T t;
        try {
            t = clazz.newInstance();
            FieldUtils.writeField(t, fields[0].getName(), primaryKey, true);
            if (Objects.nonNull(organizationId)) {
                FieldUtils.writeField(t, FIELD_ORGANIZATION_ID, organizationId, true);
            }
            if (Objects.nonNull(projectId)) {
                FieldUtils.writeField(t, FIELD_PROJECT_ID, projectId, true);
            }
        } catch (InstantiationException | IllegalAccessException e) {
            LOGGER.error("无法实例化对象，请检查, e.message: [{}], e.trace: [{}]", e.getMessage(), e.getStackTrace());
            throw new CommonException(BaseConstants.ErrorCode.ERROR);
        }
        T update = mapper.selectOne(t);
        if (Objects.isNull(update)) {
            LOGGER.error("数据不存在,请检查。实体对象: [{}] ", t);
            return null;
        }
        mapper.updateOptional(update);
        return update;
    }

    public static void updateIssueLastUpdateInfoForIssueLinks(IssueConvertDTO issueConvertDTO, List<IssueLinkDTO> issueLinkDTOS) {
        if (Objects.isNull(issueMapper)) {
            issueMapper = ApplicationContextHelper.getContext().getBean(IssueMapper.class);
        }
        IssueDTO issueDTO = new IssueDTO();
        issueDTO.setIssueId(issueConvertDTO.getIssueId());
        issueDTO.setObjectVersionNumber(issueConvertDTO.getObjectVersionNumber());
        if (ObjectUtils.isEmpty(issueMapper.selectOne(issueDTO))) {
            throw new CommonException("error.issue.objectVersionNumber.illegal");
        }
        if (CollectionUtils.isEmpty(issueLinkDTOS)) {
            return;
        }
        Set<Long> issueIds = new HashSet<>();
        issueLinkDTOS.forEach(link -> {
            if (!issueIds.contains(link.getIssueId())) {
                issueIds.add(link.getIssueId());
                updateIssueLastUpdateInfo(link.getIssueId(), link.getProjectId());
            }
            if (!issueIds.contains(link.getLinkedIssueId())) {
                issueIds.add(link.getLinkedIssueId());
                updateIssueLastUpdateInfo(link.getLinkedIssueId(), link.getProjectId());
            }
        });
        issueConvertDTO.setObjectVersionNumber(issueConvertDTO.getObjectVersionNumber() + 1L);
    }
}
