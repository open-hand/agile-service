package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.WorkCalendarHolidayRefVO;
import io.choerodon.agile.app.assembler.WorkCalendarHolidayRefAssembler;
import io.choerodon.agile.app.service.WorkCalendarHolidayRefService;
import io.choerodon.agile.infra.dto.WorkCalendarHolidayRefDTO;
import io.choerodon.agile.infra.feign.RemoteIamFeignClient;
import io.choerodon.agile.infra.utils.DateUtil;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.List;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/10/9
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class WorkCalendarHolidayRefServiceImpl implements WorkCalendarHolidayRefService {

    @Autowired
    private WorkCalendarHolidayRefAssembler workCalendarHolidayRefAssembler;
    @Autowired
    private RemoteIamFeignClient remoteIamFeignClient;
    @Autowired
    private ModelMapper modelMapper;

    private static final String DATE_FORMAT = "yyyy-MM-dd";
    private static final Logger LOGGER = LoggerFactory.getLogger(WorkCalendarHolidayRefServiceImpl.class);
    private static final String PARSE_EXCEPTION = "ParseException{}";


    @Override
    public List<WorkCalendarHolidayRefVO> queryByYearIncludeLastAndNext(Integer year) {
        return formatAndSortToDTO(modelMapper.map(remoteIamFeignClient.queryByYearIncludeLastAndNext(0L, year).getBody(),
                new TypeToken<List<WorkCalendarHolidayRefDTO>>() {
                }.getType()));
    }

    private List<WorkCalendarHolidayRefVO> formatAndSortToDTO(List<WorkCalendarHolidayRefDTO> workCalendarHolidayRefDTOS) {
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat(DATE_FORMAT);
        workCalendarHolidayRefDTOS.forEach(workCalendarHolidayRefDO -> {
            try {
                workCalendarHolidayRefDO.setHoliday(simpleDateFormat.format(simpleDateFormat.parse(workCalendarHolidayRefDO.getHoliday())));
            } catch (ParseException e) {
                LOGGER.warn(PARSE_EXCEPTION, e);
            }
        });
        return workCalendarHolidayRefAssembler.toTargetList(DateUtil.stringDateCompare().
                sortedCopy(workCalendarHolidayRefDTOS), WorkCalendarHolidayRefVO.class);
    }
}
