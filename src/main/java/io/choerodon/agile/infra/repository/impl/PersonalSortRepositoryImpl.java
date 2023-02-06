package io.choerodon.agile.infra.repository.impl;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Repository;
import org.springframework.util.Assert;

import io.choerodon.agile.api.vo.IssuePersonalSortVO;
import io.choerodon.agile.domain.repository.PersonalSortRepository;
import io.choerodon.agile.infra.dto.IssuePersonalSortDTO;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;

import org.hzero.core.base.BaseConstants;
import org.hzero.mybatis.base.impl.BaseRepositoryImpl;

/**
 * 用户个性化排序规则 Repository Impl
 * @author gaokuo.dai@zknow.com 2023-02-02
 */
@Repository
public class PersonalSortRepositoryImpl extends BaseRepositoryImpl<IssuePersonalSortDTO> implements PersonalSortRepository {

    @Override
    public List<IssuePersonalSortVO> listByBusinessType(Long organizationId, Long projectId, String businessType) {
        Assert.notNull(organizationId, BaseConstants.ErrorCode.NOT_NULL);
        Assert.hasText(businessType, BaseConstants.ErrorCode.NOT_NULL);
        Long userId = DetailsHelper.getUserDetails().getUserId();
        if(projectId == null) {
            projectId = BaseConstants.DEFAULT_TENANT_ID;
        }
        if(!BaseConstants.DEFAULT_TENANT_ID.equals(projectId)) {
            Assert.isTrue(Objects.equals(organizationId, ConvertUtil.getOrganizationId(projectId)), BaseConstants.ErrorCode.DATA_INVALID);
        }

        IssuePersonalSortDTO personalSort = new IssuePersonalSortDTO();
        personalSort.setProjectId(projectId);
        personalSort.setOrganizationId(organizationId);
        personalSort.setUserId(userId);
        personalSort.setBusinessType(businessType);
        IssuePersonalSortDTO sort = this.selectOne(personalSort);

        if (sort == null) {
            return Collections.emptyList();
        } else {
            String json = sort.getSortJson();
            if(StringUtils.isBlank(json)) {
                return Collections.emptyList();
            }
            ObjectMapper objectMapper = new ObjectMapper();
            try {
                return objectMapper.readValue(json, new TypeReference<List<IssuePersonalSortVO>>() {});
            } catch (IOException e) {
                throw new CommonException("error.gantt.sortJson.deserialization", e);
            }
        }
    }
}
