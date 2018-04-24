--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.7
-- Dumped by pg_dump version 9.6.7

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: images; Type: TABLE; Schema: public; Owner: catbot
--

CREATE TABLE images (
    sha text,
    original_url text,
    breed_prediction character varying(64),
    prediction_confidence double precision,
    size_bytes integer
);


ALTER TABLE images OWNER TO catbot;

--
-- Name: source_subreddits; Type: TABLE; Schema: public; Owner: catbot
--

CREATE TABLE source_subreddits (
    name character varying(64),
    high_water_mark character varying(32),
    auto_prediction character varying(64)
);


ALTER TABLE source_subreddits OWNER TO catbot;

--
-- Name: images images_path_key; Type: CONSTRAINT; Schema: public; Owner: catbot
--

ALTER TABLE ONLY images
    ADD CONSTRAINT images_path_key UNIQUE (sha);


--
-- Name: source_subreddits source_subreddits_name_key; Type: CONSTRAINT; Schema: public; Owner: catbot
--

ALTER TABLE ONLY source_subreddits
    ADD CONSTRAINT source_subreddits_name_key UNIQUE (name);


--
-- PostgreSQL database dump complete
--

