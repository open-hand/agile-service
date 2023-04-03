package io.choerodon.agile.app.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.vo.PersonalFilterVO;
import io.choerodon.agile.api.vo.search.SearchParamVO;
import io.choerodon.agile.app.service.PersonalFilterService;
import io.choerodon.agile.infra.dto.PersonalFilterDTO;
import io.choerodon.agile.infra.enums.PersonalFilterTypeCode;
import io.choerodon.agile.infra.mapper.PersonalFilterMapper;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;

import org.hzero.core.base.BaseConstants;
import org.hzero.core.util.JsonUtils;
import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;

/**
 * @author shinan.chen
 * @since 2019/2/25
 */
@Service
public class PersonalFilterServiceImpl implements PersonalFilterService {

    public static final String V1 = "v1";
    public static final String V2 = "v2";

    @Autowired
    private PersonalFilterMapper personalFilterMapper;

    public static final String UPDATE_ERROR = "error.personalFilter.update";
    public static final String DELETE_ERROR = "error.personalFilter.deleteById";
    public static final String NOT_FOUND_ERROR = "error.personalFilter.notFound";
    public static final String NAME_ERROR = "error.personalFilter.nameNotNull";
    public static final String INSERT_ERROR = "error.personalFilter.create";
    public static final String NAME_EXIST = "error.personalFilter.nameExist";

    @Autowired
    private ModelMapper modelMapper;


    @Override
    public PersonalFilterVO queryById(Long organizationId, Long projectId, Long filterId, String version) {
        PersonalFilterDTO personalFilter = new PersonalFilterDTO();
        personalFilter.setFilterId(filterId);
        personalFilter.setProjectId(projectId);
        personalFilter.setProjectId(organizationId);
        PersonalFilterDTO personalFilterDTO = personalFilterMapper.selectByPrimaryKey(personalFilter);
        if (personalFilterDTO == null) {
            throw new CommonException(NOT_FOUND_ERROR);
        }
        return convertPersonFilterDtoToVo(version, personalFilterDTO);
    }

    @Override
    public PersonalFilterVO create(Long organizationId, Long projectId, PersonalFilterVO personalFilterVO, String version) {
        String filterTypeCode = personalFilterVO.getFilterTypeCode();
        PersonalFilterTypeCode.checkTypeCodeValidOrThrow(filterTypeCode);
        if (personalFilterVO.getName() == null || personalFilterVO.getName().equals(StringUtils.EMPTY)) {
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
        PersonalFilterDTO personalFilterDTO = modelMapper.map(personalFilterVO, PersonalFilterDTO.class);
        setJsonByVersion(personalFilterVO, version, personalFilterDTO, true);
        if (personalFilterMapper.insert(personalFilterDTO) != 1) {
            throw new CommonException(INSERT_ERROR);
        }
        return queryById(organizationId, projectId, personalFilterDTO.getFilterId(), version);
    }

    private void setJsonByVersion(PersonalFilterVO personalFilterVO,
                                  String version,
                                  PersonalFilterDTO personalFilterDTO,
                                  boolean isCreated) {
        if (V1.equals(version)) {
            personalFilterDTO.setFilterJson(EncryptionUtils.handlerPersonFilterJson(personalFilterVO.getFilterJson(), false));
            personalFilterDTO.setAdvancedFilterJson(StringUtils.EMPTY);
        } else if (V2.equals(version)) {
            SearchParamVO searchParamVO = personalFilterVO.getSearchParamVO();
            String advancedFilterJson = isCreated ? StringUtils.EMPTY : null;
            if (searchParamVO != null) {
                advancedFilterJson = JsonUtils.toJson(searchParamVO);
            }
            personalFilterDTO.setAdvancedFilterJson(advancedFilterJson);
            personalFilterDTO.setFilterJson(isCreated ? StringUtils.EMPTY : null);
        } else {
            throw new CommonException("error.illegal.person.filter.version");
        }
    }

    @Override
    public PersonalFilterVO update(Long organizationId,
                                   Long projectId,
                                   Long filterId,
                                   PersonalFilterVO personalFilterVO,
                                   String version) {
        PersonalFilterDTO personalFilter = personalFilterMapper.selectByPrimaryKey(filterId);
        Assert.notNull(personalFilter, NOT_FOUND_ERROR);
        Long userId = Optional.ofNullable(DetailsHelper.getUserDetails()).map(CustomUserDetails::getUserId).orElseThrow(() -> new CommonException(BaseConstants.ErrorCode.NOT_LOGIN));
        if (!Objects.isNull(personalFilterVO.getName())) {
            checkUpdateName(organizationId, projectId, userId, filterId, personalFilterVO.getName(), personalFilter.getFilterTypeCode());
        }
        personalFilterVO.setFilterId(filterId);
        PersonalFilterDTO personalFilterDTO = modelMapper.map(personalFilterVO, PersonalFilterDTO.class);
        setJsonByVersion(personalFilterVO, version, personalFilterDTO, false);
        if (!ObjectUtils.isEmpty(personalFilterVO.getDefault()) && Boolean.TRUE.equals(personalFilterVO.getDefault())) {
            personalFilterMapper.updateDefault(organizationId, projectId, userId, false, null, personalFilter.getFilterTypeCode());
        }
        if (personalFilterMapper.updateByPrimaryKeySelective(personalFilterDTO) != 1) {
            throw new CommonException(UPDATE_ERROR);
        }
        return queryById(organizationId, projectId, filterId, version);
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
    public List<PersonalFilterVO> listByUserId(Long organizationId,
                                               Long projectId,
                                               Long userId,
                                               String searchStr,
                                               String filterTypeCode,
                                               String version) {
        PersonalFilterTypeCode.checkTypeCodeValidOrThrow(filterTypeCode);
        List<PersonalFilterDTO> personalFilterList = personalFilterMapper.queryByProjectIdAndUserId(organizationId, projectId, userId, searchStr, filterTypeCode, version);
        List<PersonalFilterVO> result = new ArrayList<>();
        for (PersonalFilterDTO dto : personalFilterList) {
            PersonalFilterVO vo = convertPersonFilterDtoToVo(version, dto);
            result.add(vo);
        }
        return result;
    }

    private PersonalFilterVO convertPersonFilterDtoToVo(String version, PersonalFilterDTO dto) {
        PersonalFilterVO vo = modelMapper.map(dto, PersonalFilterVO.class);
        if (V1.equals(version)) {
            vo.setFilterJson(EncryptionUtils.handlerPersonFilterJson(dto.getFilterJson(), true));
        } else if (V2.equals(version)) {
            String json = dto.getAdvancedFilterJson();
            if (!StringUtils.isEmpty(json)) {
                SearchParamVO searchParamVO = JsonUtils.fromJson(json, SearchParamVO.class);
                vo.setSearchParamVO(searchParamVO);
            }
        }
        return vo;
    }

    @Override
    public boolean nameIsExist(Long organizationId, Long projectId, Long userId, String name, String filterTypeCode, Long filterId) {
        PersonalFilterTypeCode.checkTypeCodeValidOrThrow(filterTypeCode);
        return this.queryRepeatRecordCount(organizationId, projectId, userId, name, filterTypeCode, filterId) > 0;
    }

    /**
     * 根据唯一性约束查询数据库中重复的记录数量
     *
     * @param organizationId 组织ID
     * @param projectId      项目ID
     * @param userId         用户ID
     * @param name           个人筛选名称
     * @param filterTypeCode 个人筛选类型
     * @param filterId       个人筛选ID(用于更新检查时排除自身)
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
        String filterTypeCode = queryById(organizationId, projectId, filterId, V1).getFilterTypeCode();
        personalFilterMapper.updateDefault(organizationId, projectId, userId, false, null, filterTypeCode);
        int result = personalFilterMapper.updateDefault(organizationId, projectId, userId, true, filterId, filterTypeCode);
        return result > 0;
    }
}
