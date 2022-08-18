package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.StatusNoticeSettingDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

/**
 * 邮件通知Mapper
 *
 * @author choerodon@choerodon.cn 2020-08-12 11:41:01
 */
public interface StatusNoticeSettingMapper extends BaseMapper<StatusNoticeSettingDTO> {

    void deleteByIssueTypeId(@Param("organizationId") Long organizationId,
                             @Param("projectId") Long projectId,
                             @Param("issueTypeId") Long issueTypeId);
}
