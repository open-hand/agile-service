package io.choerodon.agile.app.service.impl;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.vo.PersonalFilterVO;
import io.choerodon.agile.app.service.PersonalFilterService;
import io.choerodon.agile.infra.dto.PersonalFilterDTO;
import io.choerodon.agile.infra.enums.PersonalFilterTypeCode;
import io.choerodon.agile.infra.mapper.PersonalFilterMapper;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;

import org.hzero.core.base.BaseConstants;
import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;

/**
 * @author shinan.chen
 * @since 2019/2/25
 */
@Service
public class PersonalFilterServiceImpl implements PersonalFilterService {

    @Autowired
    private PersonalFilterMapper personalFilterMapper;

    public static final String UPDATE_ERROR = "error.personalFilter.update";
    public static final String DELETE_ERROR = "error.personalFilter.deleteById";
    public static final String NOT_FOUND_ERROR = "error.personalFilter.notFound";
    public static final String NAME_ERROR = "error.personalFilter.nameNotNull";
    public static final String INSERT_ERROR = "error.personalFilter.create";
    public static final String NAME_EXIST = "error.personalFilter.nameExist";
    public static final String TYPE_CODE_ILLEGAL = "error.personalFilter.typeCode.illegal";
    public static final String TYPE_CODE_NOT_NULL = "error.personalFilter.typeCode.notNull";
    private static final String[] FILTER_TYPE_CODES = {
            PersonalFilterTypeCode.AGILE_ISSUE,
            PersonalFilterTypeCode.AGILE_WORK_HOURS,
            PersonalFilterTypeCode.RISK_ISSUE,
            PersonalFilterTypeCode.FEATURE_ISSUE,
            PersonalFilterTypeCode.WATERFALL_ISSUE,
    };

    @Autowired
    private ModelMapper modelMapper;


    @Override
    public PersonalFilterVO queryById(Long organizationId, Long projectId, Long filterId) {
        PersonalFilterDTO personalFilter = new PersonalFilterDTO();
        personalFilter.setFilterId(filterId);
        personalFilter.setProjectId(projectId);
        personalFilter.setProjectId(organizationId);
        PersonalFilterDTO personalFilterDTO = personalFilterMapper.selectByPrimaryKey(personalFilter);
        if (personalFilterDTO == null) {
            throw new CommonException(NOT_FOUND_ERROR);
        }
        personalFilterDTO.setFilterJson(EncryptionUtils.handlerPersonFilterJson(personalFilterDTO.getFilterJson(),true));
        return modelMapper.map(personalFilterDTO, PersonalFilterVO.class);
    }

    @Override
    public PersonalFilterVO create(Long organizationId, Long projectId, PersonalFilterVO personalFilterVO) {
        String filterTypeCode = personalFilterVO.getFilterTypeCode();
        checkFilterTypeCode(filterTypeCode);
        if (personalFilterVO.getName() == null || personalFilterVO.getName().equals("")) {
            throw new CommonException(NAME_ERROR);
        }
        CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
        Long userId = customUserDetails.getUserId();
        Assert.isTrue(
                !this.nameIsExist(organizationId, projectId, userId, personalFilterVO.getName(), filterTypeCode, null),
                NAME_EXIST
        );
        if (!ObjectUtils.isEmpty(personalFilterVO.getDefault()) && Boolean.TRUE.equals(personalFilterVO.getDefault())) {
            personalFilterMapper.updateDefault(organizationId, projectId, userId, false, null, filterTypeCode);
        }
        personalFilterVO.setUserId(userId);
        personalFilterVO.setProjectId(projectId);
        personalFilterVO.setOrganizationId(organizationId);
        personalFilterVO.setFilterJson(EncryptionUtils.handlerPersonFilterJson(personalFilterVO.getFilterJson(),false));
        PersonalFilterDTO personalFilterDTO = modelMapper.map(personalFilterVO, PersonalFilterDTO.class);
        if (personalFilterMapper.insert(personalFilterDTO) != 1) {
            throw new CommonException(INSERT_ERROR);
        }
        return queryById(organizationId, projectId, personalFilterDTO.getFilterId());
    }

    private void checkFilterTypeCode(String filterTypeCode) {
        if (Objects.isNull(filterTypeCode) || Objects.equals("", filterTypeCode)) {
            throw new CommonException(TYPE_CODE_NOT_NULL);
        }
        if (!Arrays.asList(FILTER_TYPE_CODES).contains(filterTypeCode)) {
            throw new CommonException(TYPE_CODE_ILLEGAL);
        }
    }

    @Override
    public PersonalFilterVO update(Long organizationId, Long projectId, Long filterId, PersonalFilterVO personalFilterVO) {
        PersonalFilterDTO dto = personalFilterMapper.selectByPrimaryKey(filterId);
        if (Objects.isNull(dto)) {
            throw new CommonException(NOT_FOUND_ERROR);
        }
        Long userId = DetailsHelper.getUserDetails().getUserId();
        if (!Objects.isNull(personalFilterVO.getName())) {
            checkUpdateName(organizationId, projectId, userId, filterId, personalFilterVO.getName(), dto.getFilterTypeCode());
        }
        personalFilterVO.setFilterId(filterId);
        PersonalFilterDTO personalFilterDTO = modelMapper.map(personalFilterVO, PersonalFilterDTO.class);
        personalFilterDTO.setFilterJson(EncryptionUtils.handlerPersonFilterJson(personalFilterDTO.getFilterJson(),false));
        if (!ObjectUtils.isEmpty(personalFilterVO.getDefault()) && Boolean.TRUE.equals(personalFilterVO.getDefault())) {
            personalFilterMapper.updateDefault(organizationId, projectId, userId, false, null, dto.getFilterTypeCode());
        }
        if (personalFilterMapper.updateByPrimaryKeySelective(personalFilterDTO) != 1) {
            throw new CommonException(UPDATE_ERROR);
        }
        return queryById(organizationId, projectId, filterId);
    }

    private void checkUpdateName(Long organizationId, Long projectId, Long userId, Long filterId, String name, String filterTypeCode) {
        Assert.isTrue(
                this.queryRepeatRecordCount(organizationId, projectId, userId, name, filterTypeCode, filterId) <= 0,
                NAME_EXIST
        );
    }

    @Override
    public void deleteById(Long organizationId, Long projectId, Long filterId) {
        PersonalFilterDTO personalFilterDTO = new PersonalFilterDTO();
        personalFilterDTO.setProjectId(projectId);
        personalFilterDTO.setOrganizationId(organizationId);
        personalFilterDTO.setFilterId(filterId);
        int isDelete = personalFilterMapper.delete(personalFilterDTO);
        if (isDelete != 1) {
            throw new CommonException(DELETE_ERROR);
        }
    }

    @Override
    public List<PersonalFilterVO> listByUserId(Long organizationId, Long projectId, Long userId, String searchStr, String filterTypeCode) {
        checkFilterTypeCode(filterTypeCode);
        List<PersonalFilterVO> list = modelMapper.map(
                personalFilterMapper.queryByProjectIdAndUserId(organizationId, projectId, userId, searchStr, filterTypeCode), new TypeToken<List<PersonalFilterVO>>() {
        }.getType());
        for (PersonalFilterVO personalFilterVO : list) {
            personalFilterVO.setFilterJson(EncryptionUtils.handlerPersonFilterJson(personalFilterVO.getFilterJson(), true));
        }
        return list;
    }

    @Override
    public boolean nameIsExist(Long organizationId, Long projectId, Long userId, String name, String filterTypeCode, Long filterId) {
        this.checkFilterTypeCode(filterTypeCode);
        return this.queryRepeatRecordCount(organizationId, projectId, userId, name, filterTypeCode, filterId) > 0;
    }

    /**
     * 根据唯一性约束查询数据库中重复的记录数量
     * @param organizationId 组织ID
     * @param projectId 项目ID
     * @param userId 用户ID
     * @param name 个人筛选名称
     * @param filterTypeCode 个人筛选类型
     * @param filterId 个人筛选ID(用于更新检查时排除自身)
     * @return 重复的记录数量
     */
    private int queryRepeatRecordCount(Long organizationId, Long projectId, Long userId, String name, String filterTypeCode, Long filterId) {
        Assert.notNull(organizationId, BaseConstants.ErrorCode.NOT_NULL);
        Assert.notNull(projectId, BaseConstants.ErrorCode.NOT_NULL);
        Assert.notNull(userId, BaseConstants.ErrorCode.NOT_NULL);
        Assert.hasText(name, BaseConstants.ErrorCode.NOT_NULL);
        Assert.hasText(filterTypeCode, BaseConstants.ErrorCode.NOT_NULL);
        return this.personalFilterMapper.selectCountByCondition(Condition.builder(PersonalFilterDTO.class).andWhere(Sqls.custom()
                .andEqualTo(PersonalFilterDTO.FIELD_ORGANIZATION_ID, organizationId)
                .andEqualTo(PersonalFilterDTO.FIELD_PROJECT_ID, projectId)
                .andEqualTo(PersonalFilterDTO.FIELD_USER_ID, userId)
                .andEqualTo(PersonalFilterDTO.FIELD_NAME, name)
                .andEqualTo(PersonalFilterDTO.FIELD_FILTER_TYPE_CODE, filterTypeCode)
                .andNotEqualTo(PersonalFilterDTO.FIELD_FILTER_ID, filterId, true)
        ).build());
    }

    @Override
    public Boolean setDefault(Long organizationId, Long projectId, Long filterId) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        String filterTypeCode = queryById(organizationId, projectId, filterId).getFilterTypeCode();
        personalFilterMapper.updateDefault(organizationId, projectId, userId, false, null, filterTypeCode);
        int result = personalFilterMapper.updateDefault(organizationId, projectId, userId, true, filterId, filterTypeCode);
        return result > 0;
    }
}
