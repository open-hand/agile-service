package io.choerodon.agile.infra.utils;

import com.google.common.collect.Ordering;
import io.choerodon.agile.infra.dto.TimeZoneWorkCalendarDTO;
import io.choerodon.agile.infra.dto.TimeZoneWorkCalendarRefDTO;
import io.choerodon.agile.infra.dto.WorkCalendarHolidayRefDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.core.exception.CommonException;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/31.
 * Email: fuqianghuang01@gmail.com
 */
@Component
public class DateUtil {

    private static final Logger LOGGER = LoggerFactory.getLogger(DateUtil.class);
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private ModelMapper modelMapper;

    private static final String DATE_FORMAT = "yyyy-MM-dd";
    private static final String PARSE_EXCEPTION = "ParseException{}";

    private DateUtil() {
    }

    /**
     * 从现有比较器返回一个
     *
     * @return Ordering
     */
    public static Ordering<WorkCalendarHolidayRefDTO> stringDateCompare() {
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT, Locale.CHINA);
        return Ordering.from((o1, o2) -> {
            int a;
            try {
                a = sdf.parse(o1.getHoliday()).compareTo(sdf.parse(o2.getHoliday()));
            } catch (ParseException e) {
                throw new CommonException(PARSE_EXCEPTION, e);
            }
            return a;
        });
    }

    public Set<Date> getWorkDays(Long organizationId, Date startTime, Date endTime) {
        Set<Date> result = new HashSet<>();
        if (startTime == null || endTime == null) {
            throw new IllegalArgumentException("date can't be null");
        } else {
            Set<Integer> year = new HashSet<>();
            TimeZoneWorkCalendarDTO timeZoneWorkCalendarDTO = baseFeignClient.queryTimeZoneDetailByOrganizationId(organizationId).getBody();
            getWorkDaysInterval(timeZoneWorkCalendarDTO, startTime, endTime, result, year);
            handleHolidays(result, year, startTime, endTime, timeZoneWorkCalendarDTO);
        }
        return result;
    }

    /**
     * 获取2个时间中的工作天数。排除周末和国家法定节假日
     *
     * @param dayOne         dayOne
     * @param dayTwo         dayTwo
     * @param holiday        要排除的日期
     * @param workday        要加班的日期
     * @param organizationId organizationId
     * @return Integer
     */
    public Integer getDaysBetweenDifferentDate(Date dayOne, Date dayTwo, List<Date> holiday, List<Date> workday, Long organizationId) {
        if (dayOne == null || dayTwo == null) {
            throw new IllegalArgumentException("date can't be null");
        } else if (isSameDay(dayOne, dayTwo)) {
            return 1;
        } else {
            Set<Integer> year = new HashSet<>();
            Set<Date> dates = new HashSet<>();
            if (dayOne.after(dayTwo)) {
                return 0;
            }
            TimeZoneWorkCalendarDTO timeZoneWorkCalendarDTO = baseFeignClient.queryTimeZoneDetailByOrganizationId(organizationId).getBody();
            Integer i = getWorkDaysInterval(timeZoneWorkCalendarDTO, dayOne, dayTwo, dates, year);
            handleHolidays(dates, year, dayOne, dayTwo, timeZoneWorkCalendarDTO);
            handleTimeZoneWorkCalendarRefRemoveAndAdd(dates, timeZoneWorkCalendarDTO, dayOne, dayTwo);
            handleExcludedDate(workday, dates);
            handleAddDate(holiday, dates, dayOne, dayTwo);
            return i - dates.size();
        }
    }

    private Integer getWorkDaysInterval(TimeZoneWorkCalendarDTO timeZoneWorkCalendarDTO, Date startDate, Date endDate, Set<Date> dates, Set<Integer> year) {
        Integer i = 0;
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(startDate);
        if (timeZoneWorkCalendarDTO != null) {
            while (calendar.getTime().getTime() <= endDate.getTime()) {
                handleSaturdayAndSunday(calendar, dates, timeZoneWorkCalendarDTO);
                year.add(calendar.get(Calendar.YEAR));
                calendar.add(Calendar.DAY_OF_YEAR, 1);
                i++;
            }
            if (calendar.getTime().getTime() > endDate.getTime() && isSameDay(calendar.getTime(), endDate)) {
                handleSaturdayAndSunday(calendar, dates, timeZoneWorkCalendarDTO);
                year.add(calendar.get(Calendar.YEAR));
                i++;
            }
            return i;
        } else {
            while (calendar.getTime().getTime() <= endDate.getTime()) {
                handleSaturdayAndSundayNoTimeZone(calendar, dates);
                year.add(calendar.get(Calendar.YEAR));
                calendar.add(Calendar.DAY_OF_YEAR, 1);
                i++;
            }
            if (calendar.getTime().getTime() > endDate.getTime() && isSameDay(calendar.getTime(), endDate)) {
                handleSaturdayAndSundayNoTimeZone(calendar, dates);
                year.add(calendar.get(Calendar.YEAR));
                i++;
            }
            return i;
        }
    }

    private void handleSaturdayAndSundayNoTimeZone(Calendar calendar, Set<Date> dates) {
        if (calendar.get(Calendar.DAY_OF_WEEK) == Calendar.SATURDAY || calendar.get(Calendar.DAY_OF_WEEK) == Calendar.SUNDAY) {
            dates.add(calendar.getTime());
        }
    }

    private void handleAddDate(List<Date> addDate, Set<Date> dates, Date startDate, Date endDate) {
        if (addDate != null && !addDate.isEmpty()) {
            dates.addAll(addDate.stream().filter(date -> (date.before(endDate) && date.after(startDate) || isSameDay(date, startDate) || isSameDay(date, endDate))).collect(Collectors.toSet()));
            handleDuplicateDate(dates);
        }
    }

    private void handleExcludedDate(List<Date> excludedDate, Set<Date> dates) {
        if (excludedDate != null && !excludedDate.isEmpty()) {
            Set<Date> remove = new HashSet<>(dates.size());
            dates.forEach(date -> excludedDate.forEach(d -> {
                if (isSameDay(d, date)) {
                    remove.add(date);
                }
            }));
            dates.removeAll(remove);
        }
    }

    /**
     * 获取时间段内的非工作日(包含周末和国家法定节假日)
     *
     * @param dayOne dayOne
     * @param dayTwo dayTwo
     * @return Date
     */
    public Set<Date> getNonWorkdaysDuring(Date dayOne, Date dayTwo, Long organizationId) {
        if (dayOne == null || dayTwo == null) {
            return new HashSet<>();
        } else if (isSameDay(dayOne, dayTwo)) {
            return handleSameDayWorkDays(dayOne, organizationId);
        } else {
            return handleDifferentDayWorkDays(dayOne, dayTwo, organizationId);
        }
    }

    private Set<Date> handleDifferentDayWorkDays(Date dayOne, Date dayTwo, Long organizationId) {
        TimeZoneWorkCalendarDTO timeZoneWorkCalendarDTO = baseFeignClient.queryTimeZoneDetailByOrganizationId(organizationId).getBody();
        Set<Integer> year = new HashSet<>();
        Set<Date> dates = new HashSet<>();
        if (dayOne.after(dayTwo)) {
            Date tmp = dayOne;
            dayOne = dayTwo;
            dayTwo = tmp;
        }
        final Date startDate = dayOne;
        final Date endDate = dayTwo;
        getDaysInterval(startDate, endDate, year, dates, timeZoneWorkCalendarDTO);
        handleHolidays(dates, year, startDate, endDate, timeZoneWorkCalendarDTO);
        handleTimeZoneWorkCalendarRefRemoveAndAdd(dates, timeZoneWorkCalendarDTO, startDate, endDate);
        return dates;
    }

    private void handleTimeZoneWorkCalendarRefRemoveAndAdd(Set<Date> dates, TimeZoneWorkCalendarDTO timeZoneWorkCalendarDTO, Date startDate, Date endDate) {
        if (timeZoneWorkCalendarDTO != null && timeZoneWorkCalendarDTO.getTimeZoneWorkCalendarRefDTOS() != null
                && !timeZoneWorkCalendarDTO.getTimeZoneWorkCalendarRefDTOS().isEmpty()) {
            Set<Date> remove = new HashSet<>(dates.size() << 1);
            Set<Date> add = new HashSet<>(dates.size() << 1);
            SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT, Locale.CHINA);
            List<TimeZoneWorkCalendarRefDTO> timeZoneDate = timeZoneWorkCalendarDTO.getTimeZoneWorkCalendarRefDTOS();
            if (dates.isEmpty()) {
                handleEmptyDates(dates, timeZoneDate, sdf, endDate, startDate);
            } else {
                handleNoEmptyDates(dates, timeZoneDate, remove, add, sdf, endDate, startDate);
            }
        }
    }

    private void handleEmptyDates(Set<Date> dates, List<TimeZoneWorkCalendarRefDTO> timeZoneDate, SimpleDateFormat sdf, Date endDate, Date startDate) {
        dates.addAll(timeZoneDate.stream().filter(timeZoneWorkCalendarRefDO -> timeZoneWorkCalendarRefDO.getStatus() == 0)
                .filter(timeZoneWorkCalendarRefDO -> {
                    boolean condition = false;
                    try {
                        Date date = sdf.parse(timeZoneWorkCalendarRefDO.getWorkDay());
                        condition = ((date).before(endDate) && date.after(startDate) || isSameDay(date, startDate) || isSameDay(date, endDate)) && timeZoneWorkCalendarRefDO.getStatus() == 0;
                    } catch (ParseException e) {
                        LOGGER.warn(PARSE_EXCEPTION, e);
                    }
                    return condition;
                }).map(timeZoneWorkCalendarRefDO -> {
                    Date date = new Date();
                    try {
                        date = sdf.parse(timeZoneWorkCalendarRefDO.getWorkDay());
                    } catch (ParseException e) {
                        LOGGER.warn(PARSE_EXCEPTION, e);
                    }
                    return date;
                }).collect(Collectors.toSet()));
    }

    private void handleNoEmptyDates(Set<Date> dates, List<TimeZoneWorkCalendarRefDTO> timeZoneDate, Set<Date> remove, Set<Date> add, SimpleDateFormat sdf, Date endDate, Date startDate) {
        dates.forEach(date -> timeZoneDate.forEach(timeZoneWorkCalendarRefDO -> {
            try {
                Date holidayDate = sdf.parse(timeZoneWorkCalendarRefDO.getWorkDay());
                if (isSameDay(holidayDate, date) && timeZoneWorkCalendarRefDO.getStatus() == 1) {
                    remove.add(date);
                    remove.add(holidayDate);
                } else {
                    boolean condition = (holidayDate.before(endDate) && holidayDate.after(startDate) || isSameDay(holidayDate, startDate) || isSameDay(holidayDate, endDate)) && timeZoneWorkCalendarRefDO.getStatus() == 0;
                    if (condition) {
                        add.add(holidayDate);
                    }
                }
            } catch (ParseException e) {
                LOGGER.warn(PARSE_EXCEPTION, e);
            }
        }));
        dates.addAll(add);
        dates.removeAll(remove);
        handleDuplicateDate(dates);
    }


    private void handleHolidays(Set<Date> dates, Set<Integer> year, Date startDate, Date endDate, TimeZoneWorkCalendarDTO timeZoneWorkCalendarDTO) {
        if (!dates.isEmpty() && timeZoneWorkCalendarDTO != null) {
            Set<WorkCalendarHolidayRefDTO> holidays = new HashSet<>();
            year.forEach(y -> holidays.addAll(new HashSet<>(
                    modelMapper.map(baseFeignClient.queryWorkCalendarHolidayRelByYear(0L, y).getBody(), new TypeToken<List<WorkCalendarHolidayRefDTO>>() {
                    }.getType()))));
            if (timeZoneWorkCalendarDTO.getUseHoliday() && !holidays.isEmpty()) {
                handleHolidaysRemoveAndAdd(dates, holidays, startDate, endDate);
            }
        } else if (dates.isEmpty() && timeZoneWorkCalendarDTO != null && timeZoneWorkCalendarDTO.getUseHoliday()) {
            Set<WorkCalendarHolidayRefDTO> holidays = new HashSet<>();
            year.forEach(y -> holidays.addAll(new HashSet<>(
                    modelMapper.map(baseFeignClient.queryWorkCalendarHolidayRelByYear(0L, y).getBody(), new TypeToken<List<WorkCalendarHolidayRefDTO>>() {
                    }.getType()))));
            if (!holidays.isEmpty()) {
                SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT, Locale.CHINA);
                dates.addAll(holidays.stream().map(holiday -> {
                    Date date = new Date();
                    try {
                        date = sdf.parse(holiday.getHoliday());
                    } catch (ParseException e) {
                        LOGGER.warn(PARSE_EXCEPTION, e);
                    }
                    return date;
                }).filter(holiday -> holiday.before(endDate) && holiday.after(startDate)).collect(Collectors.toSet()));
            }
        }
    }

    public void handleDuplicateDate(Set<Date> dates) {
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat(DATE_FORMAT);
        Set<String> date = dates.stream().map(simpleDateFormat::format).collect(Collectors.toSet());
        Set<Date> datesSet = date.stream().map(s -> {
            Date d = new Date();
            try {
                d = simpleDateFormat.parse(s);
            } catch (ParseException e) {
                LOGGER.warn(PARSE_EXCEPTION, e);
            }
            return d;
        }).collect(Collectors.toSet());
        dates.clear();
        dates.addAll(datesSet);
    }

    private void handleHolidaysRemoveAndAdd(Set<Date> dates, Set<WorkCalendarHolidayRefDTO> holidays, Date startDate, Date endDate) {
        Set<Date> remove = new HashSet<>(dates.size() << 1);
        Set<Date> add = new HashSet<>(dates.size() << 1);
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT, Locale.CHINA);
        dates.forEach(date -> holidays.forEach(holiday -> {
            try {
                Date holidayDate = sdf.parse(holiday.getHoliday());
                if (isSameDay(holidayDate, date) && holiday.getStatus() == 1) {
                    remove.add(date);
                    remove.add(holidayDate);
                } else if (holidayDate.before(endDate) && holidayDate.after(startDate) && holiday.getStatus() == 0) {
                    add.add(holidayDate);
                }
            } catch (ParseException e) {
                LOGGER.warn(PARSE_EXCEPTION, e);
            }
        }));
        dates.addAll(add);
        dates.removeAll(remove);
        handleDuplicateDate(dates);
    }


    private void getDaysInterval(Date startDate, Date endDate, Set<Integer> year, Set<Date> dates, TimeZoneWorkCalendarDTO timeZoneWorkCalendarDTO) {
        if (timeZoneWorkCalendarDTO != null) {
            handleTimeZoneDaysInterval(endDate, startDate, timeZoneWorkCalendarDTO, dates, year);
        } else {
            handleDaysInterval(endDate, startDate, dates, year);
        }
    }

    private void handleDaysInterval(Date endDate, Date startDate, Set<Date> dates, Set<Integer> year) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(startDate);
        Calendar endCalendar = Calendar.getInstance();
        endCalendar.setTime(endDate);
        while (calendar.getTime().getTime() <= endDate.getTime()) {
            handleSaturdayAndSundayNoTimeZone(calendar, dates);
            year.add(calendar.get(Calendar.YEAR));
            calendar.add(Calendar.DAY_OF_YEAR, 1);
        }
        if (calendar.getTime().getTime() > endDate.getTime() && isSameDay(calendar.getTime(), endDate)) {
            handleSaturdayAndSundayNoTimeZone(calendar, dates);
            year.add(calendar.get(Calendar.YEAR));
        }
    }

    private void handleTimeZoneDaysInterval(Date endDate, Date startDate, TimeZoneWorkCalendarDTO timeZoneWorkCalendarDTO, Set<Date> dates, Set<Integer> year) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(startDate);
        Calendar endCalendar = Calendar.getInstance();
        endCalendar.setTime(endDate);
        while (calendar.getTime().getTime() <= endDate.getTime() || isSameDay(calendar, endCalendar)) {
            handleSaturdayAndSunday(calendar, dates, timeZoneWorkCalendarDTO);
            year.add(calendar.get(Calendar.YEAR));
            calendar.add(Calendar.DAY_OF_YEAR, 1);
        }
        if (calendar.getTime().getTime() > endDate.getTime() && isSameDay(calendar.getTime(), endDate)) {
            handleSaturdayAndSundayNoTimeZone(calendar, dates);
            year.add(calendar.get(Calendar.YEAR));
        }
    }


    private Set<Date> handleSameDayWorkDays(Date date, Long organizationId) {
        TimeZoneWorkCalendarDTO timeZoneWorkCalendarDTO = baseFeignClient.queryTimeZoneDetailByOrganizationId(organizationId).getBody();
        Set<Date> dates = new HashSet<>(1);
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        //处理周六周天设置
        if (timeZoneWorkCalendarDTO != null) {
            handleSaturdayAndSunday(calendar, dates, timeZoneWorkCalendarDTO);
            //处理节假日设置
            handleSameDayHoliday(calendar, timeZoneWorkCalendarDTO, dates, date);
            //处理自定义节假日
            handleSameDayTimeZoneCalendarRef(timeZoneWorkCalendarDTO, date, dates);
        } else {
            if (calendar.get(Calendar.DAY_OF_WEEK) == Calendar.SATURDAY || calendar.get(Calendar.DAY_OF_WEEK) == Calendar.SUNDAY) {
                dates.add(calendar.getTime());
            }
        }
        return dates;
    }

    private void handleSameDayTimeZoneCalendarRef(TimeZoneWorkCalendarDTO timeZoneWorkCalendarDTO, Date date, Set<Date> dates) {
        if (timeZoneWorkCalendarDTO.getTimeZoneWorkCalendarRefDTOS() != null && !timeZoneWorkCalendarDTO.getTimeZoneWorkCalendarRefDTOS().isEmpty()) {
            SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT, Locale.CHINA);
            timeZoneWorkCalendarDTO.getTimeZoneWorkCalendarRefDTOS().forEach(timeZoneWorkCalendarRefDO -> {
                try {
                    if (isSameDay(date, sdf.parse(timeZoneWorkCalendarRefDO.getWorkDay()))) {
                        if (timeZoneWorkCalendarRefDO.getStatus() == 1) {
                            dates.add(date);
                        } else {
                            dates.remove(date);
                        }
                    }
                } catch (ParseException e) {
                    LOGGER.warn(PARSE_EXCEPTION, e);
                }
            });
        }
    }


    private void handleSameDayHoliday(Calendar calendar, TimeZoneWorkCalendarDTO timeZoneWorkCalendarDTO, Set<Date> dates, Date date) {
        if (timeZoneWorkCalendarDTO.getUseHoliday()) {
            Set<WorkCalendarHolidayRefDTO> holidays = (new HashSet<>(
                    modelMapper.map(baseFeignClient.queryWorkCalendarHolidayRelByYear(0L, calendar.get(Calendar.YEAR)).getBody(), new TypeToken<List<WorkCalendarHolidayRefDTO>>() {
                    }.getType())));
            if (!holidays.isEmpty()) {
                try {
                    SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT, Locale.CHINA);
                    for (WorkCalendarHolidayRefDTO holiday : holidays) {
                        handleSameDayAddAndRemove(sdf, date, holiday, dates);
                    }
                } catch (ParseException e) {
                    LOGGER.warn(PARSE_EXCEPTION, e);
                }
            }
        }
    }

    private void handleSameDayAddAndRemove(SimpleDateFormat sdf, Date date, WorkCalendarHolidayRefDTO holiday, Set<Date> dates) throws ParseException {
        if (isSameDay(date, sdf.parse(holiday.getHoliday()))) {
            if (holiday.getStatus() == 0) {
                dates.add(date);
            } else {
                dates.remove(date);
            }
        }
    }

    private void handleSaturdayAndSunday(Calendar calendar, Set<Date> dates, TimeZoneWorkCalendarDTO timeZoneWorkCalendarDTO) {
        Boolean condition = (calendar.get(Calendar.DAY_OF_WEEK) == Calendar.SATURDAY && !timeZoneWorkCalendarDTO.getSaturdayWork())
                || (calendar.get(Calendar.DAY_OF_WEEK) == Calendar.SUNDAY && !timeZoneWorkCalendarDTO.getSundayWork());
        if (condition) {
            dates.add(calendar.getTime());
        }
    }

    public static boolean isSameDay(Date date1, Date date2) {
        if (date1 != null && date2 != null) {
            Calendar cal1 = Calendar.getInstance();
            cal1.setTime(date1);
            Calendar cal2 = Calendar.getInstance();
            cal2.setTime(date2);
            return isSameDay(cal1, cal2);
        } else {
            throw new IllegalArgumentException("The date must not be null");
        }
    }

    private static boolean isSameDay(Calendar cal1, Calendar cal2) {
        if (cal1 != null && cal2 != null) {
            return cal1.get(Calendar.ERA) == cal2.get(Calendar.ERA) && cal1.get(Calendar.YEAR) == cal2.get(Calendar.YEAR) && cal1.get(Calendar.DAY_OF_YEAR) == cal2.get(Calendar.DAY_OF_YEAR);
        } else {
            throw new IllegalArgumentException("The date must not be null");
        }
    }

    public static int differentDaysByMillisecond(Date startdate, Date endDate) {
        return (int) ((endDate.getTime() - startdate.getTime()) / (1000 * 3600 * 24));
    }

    /**
     * 计算两个日期的日期差
     * @param startDate
     * @param endDate
     * @return
     */
    public static int differentDays(Date startDate, Date endDate) {
        if (startDate.after(endDate)) {
            return 0;
        }
        Calendar startCal = Calendar.getInstance();
        startCal.setTime(startDate);

        Calendar endCal = Calendar.getInstance();
        endCal.setTime(endDate);
        int startDay = startCal.get(Calendar.DAY_OF_YEAR);
        int endDay = endCal.get(Calendar.DAY_OF_YEAR);

        int startYear = startCal.get(Calendar.YEAR);
        int endYear = endCal.get(Calendar.YEAR);
        if (startYear != endYear)   //不同年
        {
            int timeDistance = 0;
            for (int i = startYear; i < endYear; i++) {
                if (i % 4 == 0 && i % 100 != 0 || i % 400 == 0)    //闰年
                {
                    timeDistance += 366;
                } else    //不是闰年
                {
                    timeDistance += 365;
                }
            }
            return endDay - startDay > 0 ? 0 : timeDistance + (endDay - startDay);
        } else    //同年
        {
            return endDay - startDay>0 ? endDay-startDay : 0;
        }

    }

    public static LocalDate StringToLocalDate(String dateStr, String formatter) {
        if (ObjectUtils.isEmpty(dateStr)) {
            return null;
        }
        DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern(formatter);
        dateTimeFormatter = dateTimeFormatter.withLocale(Locale.getDefault());
        return LocalDate.parse(dateStr, dateTimeFormatter);
    }

    public static LocalDateTime StringToLocalDateTime(String dateStr, String formatter) {
        if (ObjectUtils.isEmpty(dateStr)) {
            return null;
        }
        DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern(formatter);
        dateTimeFormatter = dateTimeFormatter.withLocale(Locale.getDefault());
        return LocalDateTime.parse(dateStr, dateTimeFormatter);
    }

    public static LocalDate dateToLocalDate(Date date) {
        if (ObjectUtils.isEmpty(date)) {
            return null;
        }
        return date.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
    }

    public static LocalDateTime dateToLocalDateTime(Date date) {
        if (ObjectUtils.isEmpty(date)) {
            return null;
        }
        return date.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
    }

    public static Date localDateToDate(LocalDate localDate) {
        if (ObjectUtils.isEmpty(localDate)) {
            return null;
        }
        return Date.from(localDate.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant());
    }

    public static Date localDateTimeToDate(LocalDateTime localDateTime) {
        if (ObjectUtils.isEmpty(localDateTime)) {
            return null;
        }
        return Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant());
    }
}
