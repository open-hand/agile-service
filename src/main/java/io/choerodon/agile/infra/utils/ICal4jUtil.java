package io.choerodon.agile.infra.utils;

import io.choerodon.core.exception.CommonException;
import net.fortuna.ical4j.data.CalendarOutputter;
import net.fortuna.ical4j.model.Calendar;
import net.fortuna.ical4j.model.component.VEvent;
import net.fortuna.ical4j.model.property.*;
import org.springframework.util.CollectionUtils;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.List;

/**
 * @author huaxin.deng@hand-china.com 2021-10-13 15:07:59
 */
public class ICal4jUtil {

    private ICal4jUtil() {}

    public static byte[] getCalendarFileByteArray(List<VEvent> events) {
        // 日历文件初始化
        Calendar cld = init();
        // 添加事件
        addEvents(cld, events);
        // 文件验证
        if (!CollectionUtils.isEmpty(events)) {
            validate(cld);
        }
        // 生成二进制流
        return getByteArray(cld);
    }

    private static Calendar init() {
        Calendar cld = new Calendar();
        cld.getProperties().add(Version.VERSION_2_0);
        cld.getProperties().add(CalScale.GREGORIAN);
        cld.getProperties().add(new ProdId("-//CHOERODON//project//ZH"));
        cld.getProperties().add(new Name("CHOERODON-Issue"));
        cld.getProperties().add(new XProperty("X-WR-CALNAME", "CHOERODON-Issue"));
        cld.getProperties().add(new XProperty("X-WR-TIMEZONE", "Asia/Shanghai"));
        return cld;
    }

    private static void addEvents(Calendar cld, List<VEvent> events) {
        if (!CollectionUtils.isEmpty(events)) {
            for (VEvent event : events) {
                cld.getComponents().add(event);
            }
        }
    }

    private static void validate(Calendar cld) {
        cld.validate();
    }

    private static byte[] getByteArray(Calendar cld) {
        CalendarOutputter co = new CalendarOutputter(false);
        try (ByteArrayOutputStream os = new ByteArrayOutputStream()) {
            co.output(cld, os);
            return os.toByteArray();
        } catch (IOException e) {
            throw new CommonException(e.getMessage(), e);
        }
    }
}
