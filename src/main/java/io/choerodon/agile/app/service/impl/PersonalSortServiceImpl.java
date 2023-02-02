package io.choerodon.agile.app.service.impl;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;

import io.choerodon.agile.api.vo.IssuePersonalSortVO;
import io.choerodon.agile.app.service.PersonalSortService;
import io.choerodon.agile.domain.repository.PersonalSortRepository;
import io.choerodon.agile.infra.dto.IssuePersonalSortDTO;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;

import org.hzero.core.base.BaseConstants;

/**
 * 用户个性化排序规则 Service Impl
 * @author gaokuo.dai@zknow.com 2023-02-02
 */
@Service
public class PersonalSortServiceImpl implements PersonalSortService {

    @Autowired
    private PersonalSortRepository personalSortRepository;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveSort(Long organizationId, Long projectId, String businessType, List<IssuePersonalSortVO> issuePersonalSorts) {
        if(CollectionUtils.isEmpty(issuePersonalSorts)) {
            return;
        }
        Assert.notNull(organizationId, BaseConstants.ErrorCode.NOT_NULL);
        Assert.hasText(businessType, BaseConstants.ErrorCode.NOT_NULL);
        Long userId = DetailsHelper.getUserDetails().getUserId();
        if(projectId == null) {
            projectId = BaseConstants.DEFAULT_TENANT_ID;
        }
        if(!BaseConstants.DEFAULT_TENANT_ID.equals(projectId)) {
            Assert.isTrue(Objects.equals(organizationId, ConvertUtil.getOrganizationId(projectId)), BaseConstants.ErrorCode.DATA_INVALID);
        }

        for (IssuePersonalSortVO sort : issuePersonalSorts) {
            String property = sort.getProperty();
            String direction = sort.getDirection();
            AssertUtilsForCommonException.notEmpty(property, "error.personal-sort.property.empty");
            AssertUtilsForCommonException.notEmpty(direction, "error.personal-sort.sort.direction.empty");
            List<String> directions = Arrays.asList("asc", "desc");
            if (!directions.contains(direction.toLowerCase())) {
                throw new CommonException("error.illegal.personal-sort.sort.direction");
            }

        }

        IssuePersonalSortDTO personalSort = new IssuePersonalSortDTO();
        personalSort.setProjectId(projectId);
        personalSort.setOrganizationId(organizationId);
        personalSort.setUserId(userId);
        personalSort.setBusinessType(businessType);
        ObjectMapper objectMapper = new ObjectMapper();
        String sortJson;
        try {
            sortJson = objectMapper.writeValueAsString(issuePersonalSorts);
        } catch (JsonProcessingException e) {
            throw new CommonException("error.personal-sort.sortJson.serialization", e);
        }
        IssuePersonalSortDTO personalSortInDb = this.personalSortRepository.selectOne(personalSort);
        if (personalSortInDb == null) {
            personalSort.setSortJson(sortJson);
            this.personalSortRepository.insertSelective(personalSort);
        } else {
            personalSortInDb.setSortJson(sortJson);
            this.personalSortRepository.updateOptional(personalSortInDb, IssuePersonalSortDTO.FIELD_SORT_JSON);
        }
    }
}
