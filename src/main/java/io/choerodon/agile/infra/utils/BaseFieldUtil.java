package io.choerodon.agile.infra.utils;

import java.lang.reflect.Field;
import java.util.Objects;

import javax.persistence.Id;

import io.choerodon.agile.infra.dto.IssueDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.core.convertor.ApplicationContextHelper;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.common.BaseMapper;
import io.choerodon.mybatis.domain.AuditDomain;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.hzero.core.base.BaseConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.Assert;

/**
 * 表基础字段处理工具类
 * @author jiaxu.cui@hand-china.com 2020/7/6 上午11:06
 */
public class BaseFieldUtil {

    private static final Logger LOGGER = LoggerFactory.getLogger(BaseFieldUtil.class);

    protected static String FIELD_ORGANIZATION_ID = "organizationId";
    protected static String FIELD_PROJECT_ID = "projectId";

    private static IssueMapper issueMapper;

    /**
     * 更新issue的最后更新时间，最后更新人
     * @param primaryKey 主键值
     * @param projectId 项目id
     */
    public static void updateIssueLastUpdateInfo(Long primaryKey, Long projectId){
        if (Objects.isNull(issueMapper)){
            issueMapper = ApplicationContextHelper.getContext().getBean(IssueMapper.class);
        }
        updateLastUpdateInfo(issueMapper, IssueDTO.class, primaryKey, null, projectId);
    }


    /**
     * 更新最后更新时间，最后更新人
     * @param mapper {@link BaseMapper}
     * @param clazz 实例类
     * @param primaryKey 主键值
     * @param organizationId 组织id
     * @param projectId 项目id
     * @param <T> 实体类
     */
    public static <T extends AuditDomain> void updateLastUpdateInfo(BaseMapper<T> mapper,
                                                                    Class<T> clazz,
                                                                    Long primaryKey,
                                                                    Long organizationId,
                                                                    Long projectId){
        Assert.notNull(primaryKey, BaseConstants.ErrorCode.DATA_INVALID);
        Assert.isTrue(Objects.nonNull(organizationId) || Objects.nonNull(projectId),
                BaseConstants.ErrorCode.DATA_INVALID);
        Field[] fields = FieldUtils.getFieldsWithAnnotation(clazz, Id.class);
        if (fields.length != 1){
            throw new CommonException(BaseConstants.ErrorCode.ERROR);
        }
        T t;
        try {
            t = clazz.newInstance();
            FieldUtils.writeField(t, fields[0].getName(), primaryKey, true);
            if(Objects.nonNull(organizationId)){
                FieldUtils.writeField(t, FIELD_ORGANIZATION_ID, organizationId, true);
            }
            if (Objects.nonNull(projectId)){
                FieldUtils.writeField(t, FIELD_PROJECT_ID, projectId, true);
            }
        } catch (InstantiationException | IllegalAccessException e) {
            LOGGER.error("无法实例化对象，请检查, e.message: [{}], e.trace: [{}]", e.getMessage(), e.getStackTrace());
            throw new CommonException(BaseConstants.ErrorCode.ERROR);
        }
        T update = mapper.selectOne(t);
        if (Objects.isNull(update)){
            LOGGER.error("数据不存在,请检查。实体对象: [{}] ", t);
            throw new CommonException(BaseConstants.ErrorCode.DATA_NOT_EXISTS);
        }
        mapper.updateOptional(update);
    }
}
