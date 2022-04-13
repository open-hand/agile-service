package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.PersonalFilterVO;
import io.choerodon.agile.app.service.PersonalFilterService;
import io.choerodon.agile.infra.dto.PersonalFilterDTO;
import io.choerodon.agile.infra.enums.PersonalFilterTypeCode;
import io.choerodon.agile.infra.mapper.PersonalFilterMapper;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

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
    public static final String NOTFOUND_ERROR = "error.personalFilter.notFound";
    public static final String NAME_ERROR = "error.personalFilter.nameNotNull";
    public static final String INSERT_ERROR = "error.personalFilter.create";
    public static final String NAME_EXIST = "error.personalFilter.nameExist";
    public static final String TYPE_CODE_ILLEGAL = "error.personalFilter.typeCode.illegal";
    public static final String TYPE_CODE_NOT_NULL = "error.personalFilter.typeCode.notNull";
    private static final String[] FILTER_TYPE_CODES = {PersonalFilterTypeCode.AGILE_ISSUE, PersonalFilterTypeCode.AGILE_WORK_HOURS, PersonalFilterTypeCode.RISK_ISSUE};

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
            throw new CommonException(NOTFOUND_ERROR);
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
        if (Boolean.TRUE.equals(checkName(organizationId, projectId, userId, personalFilterVO.getName(), filterTypeCode))) {
            throw new CommonException(NAME_EXIST);
        }
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
            throw new CommonException(NOTFOUND_ERROR);
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
        PersonalFilterDTO personalFilterDTO = new PersonalFilterDTO();
        personalFilterDTO.setProjectId(projectId);
        personalFilterDTO.setOrganizationId(organizationId);
        personalFilterDTO.setUserId(userId);
        personalFilterDTO.setName(name);
        personalFilterDTO.setFilterTypeCode(filterTypeCode);
        List<PersonalFilterDTO> list = personalFilterMapper.select(personalFilterDTO);
        if (list.size() > 1 || (list.size() == 1 && !list.get(0).getFilterId().equals(filterId))) {
            throw new CommonException(NAME_EXIST);
        }
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
        list.forEach(v -> v.setFilterJson(EncryptionUtils.handlerPersonFilterJson(v.getFilterJson(),true)));
        return list;
    }

    @Override
    public Boolean checkName(Long organizationId, Long projectId, Long userId, String name, String filterTypeCode) {
        checkFilterTypeCode(filterTypeCode);
        PersonalFilterDTO personalFilterDTO = new PersonalFilterDTO();
        personalFilterDTO.setProjectId(projectId);
        personalFilterDTO.setOrganizationId(organizationId);
        personalFilterDTO.setUserId(userId);
        personalFilterDTO.setName(name);
        personalFilterDTO.setFilterTypeCode(filterTypeCode);
        List<PersonalFilterDTO> list = personalFilterMapper.select(personalFilterDTO);
        return list != null && !list.isEmpty();
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
