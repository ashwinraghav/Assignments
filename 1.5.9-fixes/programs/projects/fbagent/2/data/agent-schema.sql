SET SQL DIALECT 3;

CREATE DATABASE 'hcc2616:agent'
USER 'SYSDBA' PASSWORD 'masterkey'
PAGE_SIZE 4096
DEFAULT CHARACTER SET WIN1251;



/* Domains definitions */

CREATE DOMAIN DBOOLEAN AS SMALLINT
DEFAULT 0
NOT NULL
CHECK ((value = 1) or (value = 0));

CREATE DOMAIN DCOUNTER AS INTEGER;

CREATE DOMAIN DDATETIME AS TIMESTAMP;

CREATE DOMAIN DMESSAGE AS VARCHAR (1024) CHARACTER SET WIN1251;

CREATE DOMAIN DNAME AS VARCHAR (64) CHARACTER SET WIN1251;

CREATE DOMAIN DREFERENCE AS INTEGER;

CREATE DOMAIN DSTATUS AS CHAR (3) CHARACTER SET WIN1251
NOT NULL
CHECK ((value = 'OK') or (value = 'RUN') or (value = 'ERR'));

CREATE DOMAIN DTEXT AS BLOB sub_type 1 segment size 255 CHARACTER SET WIN1251;

CREATE DOMAIN DTIMESTAMP AS TIMESTAMP
DEFAULT current_timestamp
NOT NULL;



/* Generators definitions */

CREATE GENERATOR GEN_CATEGORY_ID ;
CREATE GENERATOR GEN_HISTORY_ID ;
CREATE GENERATOR GEN_RUN_ID ;
CREATE GENERATOR GEN_SCHEDULE_ID ;
CREATE GENERATOR GEN_STEP_ID ;
CREATE GENERATOR GEN_TASK_ID ;
SET GENERATOR GEN_CATEGORY_ID TO 5;

SET GENERATOR GEN_HISTORY_ID TO 9202;

SET GENERATOR GEN_RUN_ID TO 83;

SET GENERATOR GEN_SCHEDULE_ID TO 16;

SET GENERATOR GEN_STEP_ID TO 22;

SET GENERATOR GEN_TASK_ID TO 19;



/* Tables definitions */

CREATE TABLE CATEGORY (
    CATEGORY_ID DREFERENCE NOT NULL,
    NAME DNAME);

CREATE TABLE HISTORY (
    HISTORY_ID DREFERENCE NOT NULL,
    PARENT_ID DREFERENCE,
    TASK_ID DREFERENCE,
    STEP_ID DREFERENCE,
    START_TIME DDATETIME NOT NULL,
    END_TIME DDATETIME,
    STATUS DSTATUS NOT NULL,
    TEXT_MESSAGE DMESSAGE);

CREATE TABLE RUN (
    RUN_ID DREFERENCE NOT NULL,
    SCHEDULE_ID DREFERENCE NOT NULL,
    LAST_RUN TIMESTAMP,
    NEXT_RUN TIMESTAMP,
    REPEAT INTEGER DEFAULT -1);

CREATE TABLE SCHEDULE (
    SCHEDULE_ID DREFERENCE NOT NULL,
    TASK_ID DREFERENCE NOT NULL,
    NAME DNAME,
    ENABLED DBOOLEAN NOT NULL,
    DEL_KIND SMALLINT DEFAULT 0 NOT NULL,
    INCREMENT TIMESTAMP NOT NULL,
    DAY_FILTER_KIND SMALLINT,
    DAY_FILTER INTEGER,
    MONTH_FILTER INTEGER,
    DOW_POS_FILTER INTEGER,
    START_TIME TIME,
    START_DATE DATE,
    END_TIME TIME,
    END_DATE DATE,
    RUN_COUNT INTEGER);

CREATE TABLE STEP (
    STEP_ID DREFERENCE NOT NULL,
    TASK_ID DREFERENCE NOT NULL,
    NAME DNAME,
    ENABLED DBOOLEAN NOT NULL,
    ITEM_ORDER SMALLINT NOT NULL,
    ON_SUCCESS INTEGER,
    ON_FAILURE INTEGER,
    DATA DTEXT NOT NULL);

CREATE TABLE TASK (
    TASK_ID DREFERENCE NOT NULL,
    NAME DNAME,
    ENABLED DBOOLEAN NOT NULL,
    CATEGORY_ID DREFERENCE,
    DESCRIPTION DTEXT,
    DATE_CREATED DTIMESTAMP NOT NULL,
    DATE_MODIFIED DDATETIME,
    NOTIFICATION DTEXT);



/* Primary keys definition */

ALTER TABLE CATEGORY ADD CONSTRAINT PK_CATEGORY PRIMARY KEY (CATEGORY_ID);
ALTER TABLE HISTORY ADD CONSTRAINT PK_HISTORY PRIMARY KEY (HISTORY_ID);
ALTER TABLE RUN ADD CONSTRAINT PK_RUN PRIMARY KEY (RUN_ID);
ALTER TABLE SCHEDULE ADD CONSTRAINT PK_SCHEDULE PRIMARY KEY (SCHEDULE_ID);
ALTER TABLE STEP ADD CONSTRAINT PK_STEP PRIMARY KEY (STEP_ID);
ALTER TABLE TASK ADD CONSTRAINT PK_TASK PRIMARY KEY (TASK_ID);


/* Unique keys definition */

ALTER TABLE STEP ADD CONSTRAINT UK_STEP_ID_OR UNIQUE (ITEM_ORDER, TASK_ID);


/* Foreign keys definition */

ALTER TABLE HISTORY ADD CONSTRAINT FK_HISTORY_PA FOREIGN KEY (PARENT_ID) REFERENCES HISTORY (HISTORY_ID) ON DELETE CASCADE;
ALTER TABLE HISTORY ADD CONSTRAINT FK_HISTORY_ST FOREIGN KEY (STEP_ID) REFERENCES STEP (STEP_ID) ON DELETE CASCADE;
ALTER TABLE HISTORY ADD CONSTRAINT FK_HISTORY_TA FOREIGN KEY (TASK_ID) REFERENCES TASK (TASK_ID) ON DELETE CASCADE;
ALTER TABLE RUN ADD CONSTRAINT FK_RUN_SC FOREIGN KEY (SCHEDULE_ID) REFERENCES SCHEDULE (SCHEDULE_ID) ON DELETE CASCADE;
ALTER TABLE SCHEDULE ADD CONSTRAINT FK_SCHEDULE FOREIGN KEY (TASK_ID) REFERENCES TASK (TASK_ID) ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE STEP ADD CONSTRAINT FK_STEP_TASK_ID FOREIGN KEY (TASK_ID) REFERENCES TASK (TASK_ID) ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE TASK ADD CONSTRAINT FK_TASK FOREIGN KEY (CATEGORY_ID) REFERENCES CATEGORY (CATEGORY_ID) ON DELETE CASCADE ON UPDATE CASCADE;


/* Indices definition */

CREATE UNIQUE INDEX CATEGORY_IDX ON CATEGORY (NAME);
CREATE INDEX FK_HISTORY_PA ON HISTORY (PARENT_ID);
CREATE INDEX FK_HISTORY_ST ON HISTORY (STEP_ID);
CREATE INDEX FK_HISTORY_TA ON HISTORY (TASK_ID);
CREATE INDEX FK_RUN_RE ON RUN (SCHEDULE_ID);
CREATE INDEX FK_RUN_SC ON RUN (SCHEDULE_ID);
CREATE INDEX FK_SCHEDULE ON SCHEDULE (TASK_ID);
CREATE INDEX FK_STEP_TASK_ID ON STEP (TASK_ID);
CREATE INDEX FK_TASK ON TASK (CATEGORY_ID);
CREATE DESCENDING INDEX IX_HISTORY_TAHI ON HISTORY (HISTORY_ID, TASK_ID);
CREATE INDEX IX_RUN_NE ON RUN (NEXT_RUN);
CREATE INDEX IX_STEP_IDOR ON STEP (TASK_ID, ITEM_ORDER);
CREATE INDEX IX_TASK_NA ON TASK (NAME);
CREATE UNIQUE INDEX UK_STEP_ID_OR ON STEP (ITEM_ORDER, TASK_ID);


SET TERM ^ ; 

/* Stored procedures definition */


/* Stored Procedure: DO_CATEGORY_CHECK */
CREATE PROCEDURE DO_CATEGORY_CHECK (
    NAME VARCHAR (64))
RETURNS (
    CATEGORY_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_CATEGORY_DEL */
CREATE PROCEDURE DO_CATEGORY_DEL (
    CATEGORY_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_CHECKORDER */
CREATE PROCEDURE DO_CHECKORDER (
    TASK_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_RUN_INS */
CREATE PROCEDURE DO_RUN_INS (
    SCHEDULE_ID INTEGER,
    LAST_RUN TIMESTAMP,
    NEXT_RUN TIMESTAMP,
    REPEAT INTEGER)
RETURNS (
    RUN_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_ENABLE_SCHEDULE */
CREATE PROCEDURE DO_ENABLE_SCHEDULE (
    SCHEDULE_ID INTEGER,
    ENABLED SMALLINT)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_ENABLE_SCHEDULES */
CREATE PROCEDURE DO_ENABLE_SCHEDULES (
    TASK_ID INTEGER,
    ENABLED SMALLINT)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_HISTORY_CHECK */
CREATE PROCEDURE DO_HISTORY_CHECK (
    HISTORY_ID INTEGER,
    PARENT_ID INTEGER,
    TASK_ID INTEGER,
    STEP_ID INTEGER,
    START_TIME TIMESTAMP,
    END_TIME TIMESTAMP,
    STATUS CHAR (3),
    TEXT_MESSAGE VARCHAR (1024))
RETURNS (
    HISTORY_ID_OUT INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_HISTORY_DEL */
CREATE PROCEDURE DO_HISTORY_DEL (
    TASK_ID INTEGER,
    MAX_SIZE INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_RUN_DEL */
CREATE PROCEDURE DO_RUN_DEL (
    RUN_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_RUN_UPD */
CREATE PROCEDURE DO_RUN_UPD (
    RUN_ID INTEGER,
    LAST_RUN TIMESTAMP,
    NEXT_RUN TIMESTAMP,
    REPEAT INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_SCHEDULE_CHECK */
CREATE PROCEDURE DO_SCHEDULE_CHECK (
    SCHEDULE_ID INTEGER,
    TASK_ID INTEGER,
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    DEL_KIND SMALLINT,
    INCREMENT TIMESTAMP,
    DAY_FILTER_KIND SMALLINT,
    DAY_FILTER INTEGER,
    MONTH_FILTER INTEGER,
    DOW_POS_FILTER INTEGER,
    START_TIME TIME,
    START_DATE DATE,
    END_TIME TIME,
    END_DATE DATE,
    RUN_COUNT INTEGER)
RETURNS (
    NEW_SCHEDULE_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_SCHEDULE_DEL */
CREATE PROCEDURE DO_SCHEDULE_DEL (
    SCHEDULE_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_STEP_CHANGEORDER */
CREATE PROCEDURE DO_STEP_CHANGEORDER (
    STEP_ID INTEGER,
    ITEM_ORDER SMALLINT)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_STEP_CHECK */
CREATE PROCEDURE DO_STEP_CHECK (
    STEP_ID INTEGER,
    TASK_ID INTEGER,
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    ITEM_ORDER SMALLINT,
    ON_SUCCESS INTEGER,
    ON_FAILURE INTEGER,
    DATA BLOB sub_type 1 segment size 255)
RETURNS (
    NEW_STEP_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_STEP_DEL */
CREATE PROCEDURE DO_STEP_DEL (
    STEP_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_TASK_CHECK */
CREATE PROCEDURE DO_TASK_CHECK (
    TASK_ID INTEGER,
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    CATEGORY_ID INTEGER,
    DESCRIPTION BLOB sub_type 1 segment size 255,
    NOTIFICATION BLOB sub_type 1 segment size 255)
RETURNS (
    NEW_TASK_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_TASK_DEL */
CREATE PROCEDURE DO_TASK_DEL (
    TASK_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_CATEGORY_LIST */
CREATE PROCEDURE QRY_CATEGORY_LIST 
RETURNS (
    CATEGORY_ID INTEGER,
    NAME VARCHAR (64))
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_HISTORY_TASK */
CREATE PROCEDURE QRY_HISTORY_TASK (
    TASK_ID INTEGER,
    SHOW_STEP SMALLINT)
RETURNS (
    ITEM_ORDER INTEGER,
    NAME VARCHAR (64),
    START_TIME TIMESTAMP,
    END_TIME TIMESTAMP,
    STATUS CHAR (3),
    TEXT_MESSAGE VARCHAR (1024))
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_RUN */
CREATE PROCEDURE QRY_RUN 
RETURNS (
    RUN_ID INTEGER,
    SCHEDULE_ID INTEGER,
    LAST_RUN TIMESTAMP,
    NEXT_RUN TIMESTAMP,
    REPEAT INTEGER,
    TASK_ID INTEGER,
    TASK_NAME VARCHAR (64),
    INCREMENT TIMESTAMP,
    DAY_FILTER_KIND SMALLINT,
    DAY_FILTER INTEGER,
    MONTH_FILTER INTEGER,
    DOW_POS_FILTER INTEGER,
    START_TIME TIME,
    START_DATE DATE,
    END_TIME TIME,
    END_DATE DATE,
    RUN_COUNT INTEGER,
    STEP_ID INTEGER,
    STEP_NAME VARCHAR (64),
    STEP_ORDER INTEGER,
    ON_SUCCESS INTEGER,
    ON_FAILURE INTEGER,
    STEP_DATA BLOB sub_type 1 segment size 25)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_SCHEDULE */
CREATE PROCEDURE QRY_SCHEDULE (
    TASK_ID INTEGER)
RETURNS (
    SCHEDULE_ID INTEGER,
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    DEL_KIND SMALLINT)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_SCHEDULES */
CREATE PROCEDURE QRY_SCHEDULES (
    TASK_ID INTEGER)
RETURNS (
    SCHEDULE_ID INTEGER,
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    DEL_KIND SMALLINT,
    INCREMENT TIMESTAMP,
    DAY_FILTER_KIND SMALLINT,
    DAY_FILTER INTEGER,
    MONTH_FILTER INTEGER,
    DOW_POS_FILTER INTEGER,
    START_TIME TIME,
    START_DATE DATE,
    END_TIME TIME,
    END_DATE DATE,
    RUN_COUNT INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_STEP */
CREATE PROCEDURE QRY_STEP (
    TASK_ID INTEGER)
RETURNS (
    STEP_ID INTEGER,
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    ITEM_ORDER SMALLINT,
    ON_SUCCESS INTEGER,
    ON_FAILURE INTEGER,
    DATA BLOB sub_type 1 segment size 255)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_STEPS */
CREATE PROCEDURE QRY_STEPS (
    TASK_ID INTEGER)
RETURNS (
    STEP_ID INTEGER,
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    ITEM_ORDER INTEGER,
    ON_SUCCESS INTEGER,
    ON_FAILURE INTEGER,
    DATA BLOB sub_type 1 segment size 255)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_TASK_LIST */
CREATE PROCEDURE QRY_TASK_LIST 
RETURNS (
    TASK_ID INTEGER,
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    CATEGORY_ID INTEGER,
    CATEGORY_NAME VARCHAR (64),
    DESCRIPTION BLOB sub_type 1 segment size 255,
    LAST_RUN TIMESTAMP,
    LAST_STATUS CHAR (3),
    NEXT_RUN TIMESTAMP,
    DATE_CREATED TIMESTAMP,
    DATE_MODIFIED TIMESTAMP,
    NOTIFICATION BLOB sub_type 1 segment size 255)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_TASK_NOTIFICATION */
CREATE PROCEDURE QRY_TASK_NOTIFICATION (
    TASK_ID INTEGER)
RETURNS (
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    NOTIFICATION BLOB sub_type 1 segment size 8)
AS
BEGIN
  EXIT;
END
^


ALTER PROCEDURE DO_CATEGORY_CHECK (
    NAME VARCHAR (64))
RETURNS (
    CATEGORY_ID INTEGER)
AS
declare variable uname varchar(64);
begin
  uname = upper(:name);

  select first 1 category_id
  from category
  where name = :uname
  into :category_id;

  if (category_id is null) then
  begin
    category_id = GEN_ID(GEN_CATEGORY_ID, 1);

    insert into category
    (category_id, name)
    values
    (:category_id, :uname);
  end
end
^


ALTER PROCEDURE DO_CATEGORY_DEL (
    CATEGORY_ID INTEGER)
AS
begin
  delete
  from category
  where category_id = :category_id;
end
^


ALTER PROCEDURE DO_CHECKORDER (
    TASK_ID INTEGER)
AS
declare variable step_id int;
declare variable item_order int;
declare variable item_order_i int;
begin
  item_order_i = 0;

  for select step_id, item_order
  from step
  where task_id = :task_id
  order by item_order
  into :step_id, :item_order do
  begin
    if (item_order <> item_order_i) then
      update step set item_order = :item_order
      where step_id = :step_id;

    item_order_i = item_order_i + 1;
  end
end
^


ALTER PROCEDURE DO_ENABLE_SCHEDULE (
    SCHEDULE_ID INTEGER,
    ENABLED SMALLINT)
AS
declare variable task_id integer;
declare variable run_id integer;
declare variable start_date date;
declare variable start_time time;
declare variable next_run timestamp;
declare variable run_count integer;
begin
  if (enabled = 1) then
  begin
    select first 1
      task_id,
      start_date,
      start_time,
      run_count
    from schedule
    where
      schedule_id = :schedule_id
    into
      :task_id,
      :start_date,
      :start_time,
      :run_count;

    if (exists (select * from run where schedule_id = :schedule_id)) then
    begin
      update run set
      next_run = '1899-12-30',
      last_run = '1899-12-30',
      repeat = :run_count
      where schedule_id = :schedule_id;
    end else
    begin
      next_run = start_date + start_time;
      execute procedure do_run_ins(schedule_id, null, next_run, run_count) returning_values :run_id;
    end
  end else
    delete from run
    where schedule_id = :schedule_id;
end
^


ALTER PROCEDURE DO_ENABLE_SCHEDULES (
    TASK_ID INTEGER,
    ENABLED SMALLINT)
AS
declare variable schedule_id integer;
declare variable enabled_s smallint;
begin
  for select
    schedule_id,
    enabled
  from schedule s
  where
    task_id = :task_id
  into
    :schedule_id,
    :enabled_s
  do
    if (enabled_s = 1) then
      execute procedure do_enable_schedule(schedule_id, enabled);
end
^


ALTER PROCEDURE DO_HISTORY_CHECK (
    HISTORY_ID INTEGER,
    PARENT_ID INTEGER,
    TASK_ID INTEGER,
    STEP_ID INTEGER,
    START_TIME TIMESTAMP,
    END_TIME TIMESTAMP,
    STATUS CHAR (3),
    TEXT_MESSAGE VARCHAR (1024))
RETURNS (
    HISTORY_ID_OUT INTEGER)
AS
begin
  if ((not history_id is null) and (history_id <> 0)) then
    update history set
      history_id = :history_id,
      end_time = :end_time,
      status = upper(:status)
    where
      history_id = :history_id;
  else
  begin
    history_id_out = gen_id(gen_history_id, 1);

    if (parent_id = 0) then parent_id = null;
    if (task_id = 0) then task_id = null;
    if (step_id = 0) then step_id = null;

    insert into history (
      history_id,
      parent_id,
      task_id,
      step_id,
      start_time,
      status,
      text_message)
    values (
      :history_id_out,
      :parent_id,
      :task_id,
      :step_id,
      :start_time,
      upper(:status),
      :text_message);
  end
end
^


ALTER PROCEDURE DO_HISTORY_DEL (
    TASK_ID INTEGER,
    MAX_SIZE INTEGER)
AS
declare variable current_count int;
declare variable history_id int;
begin
  if (max_size > 0) then
  begin
    select count(*) from history
    where task_id = :task_id and parent_id is null
    into :current_count;

    if (current_count > max_size) then
      for select first (:current_count - :max_size) history_id
      from history
      where task_id = :task_id and parent_id is null
      order by history_id
      into :history_id do
        delete from history where history_id = :history_id;
  end else
    delete from history where task_id = :task_id;
end
^


ALTER PROCEDURE DO_RUN_DEL (
    RUN_ID INTEGER)
AS
BEGIN
  DELETE FROM RUN
  WHERE
    (RUN_ID = :RUN_ID);
END
^


ALTER PROCEDURE DO_RUN_INS (
    SCHEDULE_ID INTEGER,
    LAST_RUN TIMESTAMP,
    NEXT_RUN TIMESTAMP,
    REPEAT INTEGER)
RETURNS (
    RUN_ID INTEGER)
AS
BEGIN
  RUN_ID = GEN_ID(GEN_RUN_ID, 1);

  INSERT INTO RUN (
    RUN_ID,
    SCHEDULE_ID,
    LAST_RUN,
    NEXT_RUN,
    REPEAT)
  VALUES (
    :RUN_ID,
    :SCHEDULE_ID,
    :LAST_RUN,
    :NEXT_RUN,
    :REPEAT);
END
^


ALTER PROCEDURE DO_RUN_UPD (
    RUN_ID INTEGER,
    LAST_RUN TIMESTAMP,
    NEXT_RUN TIMESTAMP,
    REPEAT INTEGER)
AS
BEGIN
  if (repeat <> 0) then
  begin
    UPDATE RUN
    SET
      LAST_RUN = :LAST_RUN,
      NEXT_RUN = :NEXT_RUN,
      REPEAT = :REPEAT
    WHERE
      (RUN_ID = :RUN_ID);
  end else
    execute procedure do_run_del(:run_id);
END
^


ALTER PROCEDURE DO_SCHEDULE_CHECK (
    SCHEDULE_ID INTEGER,
    TASK_ID INTEGER,
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    DEL_KIND SMALLINT,
    INCREMENT TIMESTAMP,
    DAY_FILTER_KIND SMALLINT,
    DAY_FILTER INTEGER,
    MONTH_FILTER INTEGER,
    DOW_POS_FILTER INTEGER,
    START_TIME TIME,
    START_DATE DATE,
    END_TIME TIME,
    END_DATE DATE,
    RUN_COUNT INTEGER)
RETURNS (
    NEW_SCHEDULE_ID INTEGER)
AS
declare variable run_id int;
declare variable next_run timestamp;
begin
  if (exists (select * from schedule where schedule_id = :schedule_id)) then
  begin
    new_schedule_id = schedule_id;

    update schedule set
      task_id = :task_id,
      name = :name,
      enabled = :enabled,
      del_kind = :del_kind,
      increment = :increment,
      day_filter_kind = :day_filter_kind,
      day_filter = :day_filter,
      month_filter = :month_filter,
      dow_pos_filter = :dow_pos_filter,
      start_time = :start_time,
      start_date = :start_date,
      end_time = :end_time,
      end_date = :end_date,
      run_count = :run_count
    where
      schedule_id = :schedule_id;
  end else
  begin
    new_schedule_id = GEN_ID(GEN_SCHEDULE_ID, 1);

    insert into schedule (
      schedule_id,
      task_id,
      name,
      enabled,
      del_kind,
      increment,
      day_filter_kind,
      day_filter,
      month_filter,
      dow_pos_filter,
      start_time,
      start_date,
      end_time,
      end_date,
      run_count)
    values (
      :new_schedule_id,
      :task_id,
      :name,
      :enabled,
      :del_kind,
      :increment,
      :day_filter_kind,
      :day_filter,
      :month_filter,
      :dow_pos_filter,
      :start_time,
      :start_date,
      :end_time,
      :end_date,
      :run_count);
  end
  
  execute procedure do_enable_schedule(new_schedule_id, enabled);
end
^


ALTER PROCEDURE DO_SCHEDULE_DEL (
    SCHEDULE_ID INTEGER)
AS
begin
  delete from schedule
  where
  schedule_id = :schedule_id;
end
^


ALTER PROCEDURE DO_STEP_CHANGEORDER (
    STEP_ID INTEGER,
    ITEM_ORDER SMALLINT)
AS
declare variable task_id int;
declare variable old_step_id integer;
declare variable old_item_order smallint;
begin
  select first 1 task_id, item_order
  from step
  where step_id = :step_id
  into :task_id, :old_item_order;

  if (not task_id is null) then
  begin
    select first 1 step_id
    from step
    where
    task_id = :task_id and
    item_order = :item_order
    into :old_step_id;

    if (not old_step_id is null) then
      update step
      set item_order = -1
      where step_id = :old_step_id;

    update step
    set item_order = :item_order
    where step_id = :step_id;

    if (not old_step_id is null) then
      update step
      set item_order = :old_item_order
      where step_id = :old_step_id;
  end 
end
^


ALTER PROCEDURE DO_STEP_CHECK (
    STEP_ID INTEGER,
    TASK_ID INTEGER,
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    ITEM_ORDER SMALLINT,
    ON_SUCCESS INTEGER,
    ON_FAILURE INTEGER,
    DATA BLOB sub_type 1 segment size 255)
RETURNS (
    NEW_STEP_ID INTEGER)
AS
begin
  execute procedure do_checkorder(task_id);

  if (exists (select * from step where step_id = :step_id)) then
  begin
    new_step_id = step_id;
    
    update step
    set
      task_id = :task_id,
      name = :name,
      enabled = :enabled,
      item_order = :item_order,
      on_success = :on_success,
      on_failure = :on_failure,
      data = :data
    where
      step_id = :step_id;
  end else
  begin
    new_step_id = GEN_ID(GEN_STEP_ID, 1);

    insert into step (
      step_id,
      task_id,
      name,
      enabled,
      item_order,
      on_success,
      on_failure,
      data)
    values (
      :new_step_id,
      :task_id,
      :name,
      :enabled,
      :item_order,
      :on_success,
      :on_failure,
      :data);
  end
end
^


ALTER PROCEDURE DO_STEP_DEL (
    STEP_ID INTEGER)
AS
BEGIN
  DELETE FROM STEP
  WHERE
    (STEP_ID = :STEP_ID);
END
^


ALTER PROCEDURE DO_TASK_CHECK (
    TASK_ID INTEGER,
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    CATEGORY_ID INTEGER,
    DESCRIPTION BLOB sub_type 1 segment size 255,
    NOTIFICATION BLOB sub_type 1 segment size 255)
RETURNS (
    NEW_TASK_ID INTEGER)
AS
begin
  if (exists (select * from task where task_id = :task_id)) then
  begin
    new_task_id = task_id;

    update task
    set
      name = :name,
      enabled = :enabled,
      category_id = :category_id,
      description = :description,
      date_modified = current_timestamp,
      notification = :notification
    where
      task_id = :task_id;
      
    execute procedure do_enable_schedules(task_id, enabled);
  end else
  begin
    new_task_id = GEN_ID(GEN_TASK_ID, 1);

    insert into task (
      task_id,
      name,
      enabled,
      category_id,
      description,
      notification)
    values (
      :new_task_id,
      :name,
      :enabled,
      :category_id,
      :description,
      :notification);
  end
end
^


ALTER PROCEDURE DO_TASK_DEL (
    TASK_ID INTEGER)
AS
BEGIN
  DELETE FROM TASK
  WHERE
    (TASK_ID = :TASK_ID);
END
^


ALTER PROCEDURE QRY_CATEGORY_LIST 
RETURNS (
    CATEGORY_ID INTEGER,
    NAME VARCHAR (64))
AS
BEGIN
  FOR
    select CATEGORY_ID, NAME from category
    order by name

    INTO
      :CATEGORY_ID,
      :NAME
  DO
  BEGIN
    SUSPEND;
  END
END
^


ALTER PROCEDURE QRY_HISTORY_TASK (
    TASK_ID INTEGER,
    SHOW_STEP SMALLINT)
RETURNS (
    ITEM_ORDER INTEGER,
    NAME VARCHAR (64),
    START_TIME TIMESTAMP,
    END_TIME TIMESTAMP,
    STATUS CHAR (3),
    TEXT_MESSAGE VARCHAR (1024))
AS
begin
  if (show_step = 0) then
  begin
    for
      select
      0,
      '(Job Outcome)',
      t.start_time,
      t.end_time,
      t.status,
      t.text_message
      from history t
      where t.task_id = :task_id and t.parent_id is null
      order by
      t.history_id desc

      into
        :item_order,
        :name,
        :start_time,
        :end_time,
        :status,
        :text_message
    do
      suspend;
  end else
  begin
    for
      select
      coalesce(s.item_order + 1, 0),
      coalesce(s.name, '(Job Outcome)'),
      t.start_time,
      t.end_time,
      t.status,
      t.text_message
      from history t
      left join step s on s.step_id = t.step_id
      where t.task_id = :task_id
      order by
      t.history_id desc,
      s.item_order
      nulls first 

      into
        :item_order,
        :name,
        :start_time,
        :end_time,
        :status,
        :text_message
    do
      suspend;
  end
end
^


ALTER PROCEDURE QRY_RUN 
RETURNS (
    RUN_ID INTEGER,
    SCHEDULE_ID INTEGER,
    LAST_RUN TIMESTAMP,
    NEXT_RUN TIMESTAMP,
    REPEAT INTEGER,
    TASK_ID INTEGER,
    TASK_NAME VARCHAR (64),
    INCREMENT TIMESTAMP,
    DAY_FILTER_KIND SMALLINT,
    DAY_FILTER INTEGER,
    MONTH_FILTER INTEGER,
    DOW_POS_FILTER INTEGER,
    START_TIME TIME,
    START_DATE DATE,
    END_TIME TIME,
    END_DATE DATE,
    RUN_COUNT INTEGER,
    STEP_ID INTEGER,
    STEP_NAME VARCHAR (64),
    STEP_ORDER INTEGER,
    ON_SUCCESS INTEGER,
    ON_FAILURE INTEGER,
    STEP_DATA BLOB sub_type 1 segment size 25)
AS
declare variable right_now timestamp;
begin
  right_now = current_timestamp;

  for
    select
      ru.run_id,
      ru.schedule_id,
      ru.last_run,
      ru.next_run,
      ru.repeat,
      re.task_id,
      re.name,
      re.increment,
      re.day_filter_kind,
      re.day_filter,
      re.month_filter,
      re.dow_pos_filter,
      re.start_time,
      re.start_date,
      re.end_time,
      re.end_date,
      re.run_count,
      st.step_id,
      st.name,
      st.item_order,
      st.on_success,
      st.on_failure,
      st.data
    from run ru
    join schedule re on re.schedule_id = ru.schedule_id and re.enabled = 1
    join step st on st.task_id = re.task_id and st.enabled = 1
    where
      ru.next_run <= :right_now
    order by
      ru.next_run,
      st.item_order
    into
      :run_id,
      :schedule_id,
      :last_run,
      :next_run,
      :repeat,
      :task_id,
      :task_name,
      :increment,
      :day_filter_kind,
      :day_filter,
      :month_filter,
      :dow_pos_filter,
      :start_time,
      :start_date,
      :end_time,
      :end_date,
      :run_count,
      :step_id,
      :step_name,
      :step_order,
      :on_success,
      :on_failure,
      :step_data
  do
  begin
    suspend;
  end
end
^


ALTER PROCEDURE QRY_SCHEDULE (
    TASK_ID INTEGER)
RETURNS (
    SCHEDULE_ID INTEGER,
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    DEL_KIND SMALLINT)
AS
BEGIN
  FOR
    SELECT
      SCHEDULE_ID,
      NAME,
      ENABLED,
      DEL_KIND
    FROM SCHEDULE
    where
      TASK_ID = :TASK_ID
    order by
      name
    INTO
      :SCHEDULE_ID,
      :NAME,
      :ENABLED,
      :DEL_KIND
  DO
      BEGIN
        SUSPEND;
      END
END
^


ALTER PROCEDURE QRY_SCHEDULES (
    TASK_ID INTEGER)
RETURNS (
    SCHEDULE_ID INTEGER,
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    DEL_KIND SMALLINT,
    INCREMENT TIMESTAMP,
    DAY_FILTER_KIND SMALLINT,
    DAY_FILTER INTEGER,
    MONTH_FILTER INTEGER,
    DOW_POS_FILTER INTEGER,
    START_TIME TIME,
    START_DATE DATE,
    END_TIME TIME,
    END_DATE DATE,
    RUN_COUNT INTEGER)
AS
BEGIN
  FOR
    select
      s.schedule_id,
      s.name,
      s.enabled,
      s.del_kind,
      s.increment,
      s.day_filter_kind,
      s.day_filter,
      s.month_filter,
      s.dow_pos_filter,
      s.start_time,
      s.start_date,
      s.end_time,
      s.end_date,
      s.run_count
    from schedule s
    where
      s.task_id = :task_id
    order by
      s.name

    INTO
      :SCHEDULE_ID,
      :NAME,
      :ENABLED,
      :DEL_KIND,
      :increment,
      :day_filter_kind,
      :day_filter,
      :month_filter,
      :dow_pos_filter,
      :start_time,
      :start_date,
      :end_time,
      :end_date,
      :run_count
  DO
  BEGIN
    SUSPEND;
  END
END
^


ALTER PROCEDURE QRY_STEP (
    TASK_ID INTEGER)
RETURNS (
    STEP_ID INTEGER,
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    ITEM_ORDER SMALLINT,
    ON_SUCCESS INTEGER,
    ON_FAILURE INTEGER,
    DATA BLOB sub_type 1 segment size 255)
AS
BEGIN
  FOR
    SELECT
      STEP_ID,
      NAME,
      ENABLED,
      ITEM_ORDER,
      ON_SUCCESS,
      ON_FAILURE,
      DATA
    FROM STEP
    where
      task_id = :task_id
    order by
      item_order
    INTO
      :STEP_ID,
      :NAME,
      :ENABLED,
      :ITEM_ORDER,
      :ON_SUCCESS,
      :ON_FAILURE,
      :DATA
  DO
  BEGIN
    SUSPEND;
  END
END
^


ALTER PROCEDURE QRY_STEPS (
    TASK_ID INTEGER)
RETURNS (
    STEP_ID INTEGER,
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    ITEM_ORDER INTEGER,
    ON_SUCCESS INTEGER,
    ON_FAILURE INTEGER,
    DATA BLOB sub_type 1 segment size 255)
AS
BEGIN
  FOR
    select
    s.step_id, s.name, s.enabled, s.item_order, s.on_success, s.on_failure, s.data
    from step s
    where s.task_id = :task_id
    order by s.item_order

    INTO
      :STEP_ID,
      :NAME,
      :ENABLED,
      :item_order,
      :ON_SUCCESS,
      :ON_FAILURE,
      :DATA
  DO
  BEGIN
    SUSPEND;
  END
END
^


ALTER PROCEDURE QRY_TASK_LIST 
RETURNS (
    TASK_ID INTEGER,
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    CATEGORY_ID INTEGER,
    CATEGORY_NAME VARCHAR (64),
    DESCRIPTION BLOB sub_type 1 segment size 255,
    LAST_RUN TIMESTAMP,
    LAST_STATUS CHAR (3),
    NEXT_RUN TIMESTAMP,
    DATE_CREATED TIMESTAMP,
    DATE_MODIFIED TIMESTAMP,
    NOTIFICATION BLOB sub_type 1 segment size 255)
AS
declare variable history_id int;
begin
  for
    select
      t.task_id,
      t.name,
      t.enabled,
      t.category_id,
      c.name,
      t.description,
      t.date_created,
      t.date_modified,
      t.notification
    from task t
    left join category c on c.category_id = t.category_id
    order by t.name
    into
      :task_id,
      :name,
      :enabled,
      :category_id,
      :category_name,
      :description,
      :date_created,
      :date_modified,
      :notification
  do
  begin
    history_id = null;

    select max(history_id)
    from history
    where task_id = :task_id and parent_id is null
    into :history_id;

    if (:history_id is null) then
    begin
      last_run = null;
      last_status = null;
    end else
      select start_time, status
      from history
      where history_id = :history_id
      into :last_run, :last_status;

    select
    max(r.next_run)
    from schedule s
    left join run r on r.schedule_id = s.schedule_id
    where s.task_id = :task_id
    into :next_run;

    suspend;
  end
end
^


ALTER PROCEDURE QRY_TASK_NOTIFICATION (
    TASK_ID INTEGER)
RETURNS (
    NAME VARCHAR (64),
    ENABLED SMALLINT,
    NOTIFICATION BLOB sub_type 1 segment size 8)
AS
BEGIN
  FOR
    select name, enabled, notification
    from task
    where task_id = :task_id

    INTO
      :NAME,
      :ENABLED,
      :NOTIFICATION
  do
    SUSPEND;
END
^


SET TERM ; ^

SET TERM ^ ;

/* Triggers definition */



/* Trigger: BI_RUN */
CREATE TRIGGER BI_RUN FOR RUN ACTIVE
BEFORE INSERT POSITION 0
AS
BEGIN
  IF (NEW.RUN_ID IS NULL) THEN
      NEW.RUN_ID = GEN_ID(GEN_RUN_ID, 1);
END
^

/* Trigger: CATEGORY_BI */
CREATE TRIGGER CATEGORY_BI FOR CATEGORY ACTIVE
BEFORE INSERT POSITION 0
AS
BEGIN
  IF (NEW.CATEGORY_ID IS NULL) THEN
      NEW.CATEGORY_ID = GEN_ID(GEN_CATEGORY_ID,1);
END
^

/* Trigger: HISTORY_BI */
CREATE TRIGGER HISTORY_BI FOR HISTORY ACTIVE
BEFORE INSERT POSITION 0
AS
BEGIN
  IF (NEW.HISTORY_ID IS NULL) THEN
      NEW.HISTORY_ID = GEN_ID(GEN_HISTORY_ID, 1);
END
^

/* Trigger: SCHEDULE_BI */
CREATE TRIGGER SCHEDULE_BI FOR SCHEDULE ACTIVE
BEFORE INSERT POSITION 0
AS
BEGIN
  IF (NEW.SCHEDULE_ID IS NULL) THEN
      NEW.SCHEDULE_ID = GEN_ID(GEN_SCHEDULE_ID,1);
END
^

/* Trigger: STEP_BI */
CREATE TRIGGER STEP_BI FOR STEP ACTIVE
BEFORE INSERT POSITION 0
AS
BEGIN
  IF (NEW.STEP_ID IS NULL) THEN
      NEW.STEP_ID = GEN_ID(GEN_STEP_ID,1);
END
^

/* Trigger: TASK_BI */
CREATE TRIGGER TASK_BI FOR TASK ACTIVE
BEFORE INSERT POSITION 0
AS
BEGIN
  IF (NEW.TASK_ID IS NULL) THEN
      NEW.TASK_ID = GEN_ID(GEN_TASK_ID,1);
END
^

SET TERM ; ^
